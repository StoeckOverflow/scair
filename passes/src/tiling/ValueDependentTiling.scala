package scair.passes.tiling

import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.{d_tensor as DTensor}
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.NatProvenance
import scair.passes.analysis.NatProductFacts
import scair.passes.analysis.NatProductFacts.FactorSelectionPolicy
import scair.transformations.RewriteMethods

import scala.collection.mutable

object ValueDependentTiling:
  enum LoopDialect:
    case Affine
    case DAffine

  enum TilingTarget:
    case ContextBand
    case ProductReduction
    case ExplicitLoop
    case MultiDimBand

  enum TilingPolicy:
    case ExactPreferred
    case GuardedOnly
    case SeparableWhenNotExact

  enum ProofSource:
    case NatMul
    case OrdinaryProduct
    case AffineSet
    case RefinedAssert
    case None

  enum TailMode:
    case Exact
    case Guarded
    case Separable

  enum ProductLoopKind:
    case ReductionOnly
    case AnyProductLoop

  enum TilingDecision:
    case Exact
    case Guarded
    case Separable

  final case class LoopDomain(
      dialect: LoopDialect,
      target: TilingTarget,
      lowerBound: Value[Attribute],
      upperBound: Value[Attribute],
      step: IntegerAttr,
      stepOperands: Seq[Value[Attribute]],
      hasIterContract: Boolean,
      loop: Operation,
  )

  final case class TilingPlan(
      decision: TilingDecision,
      target: TilingTarget,
      proofSource: ProofSource,
  )

  final case class TileSpec(
      fullUpperBound: Value[Attribute],
      tileSize: Value[Attribute],
      prelude: Seq[Operation] = Seq.empty,
      proofSource: ProofSource = ProofSource.None,
  )

  private final case class TilingProofs(
      positiveTileSize: Option[ProofSource],
      exactDivisibility: Option[ProofSource],
      fullTileFitsGuard: Option[ProofSource],
  )

  private trait TilingFactProvider:
    def tileSpec(domain: LoopDomain): Option[TileSpec]
    def provePositive(domain: LoopDomain, value: Value[Attribute]): Option[ProofSource] = None
    def proveExactDivisibility(domain: LoopDomain, spec: TileSpec): Option[ProofSource] =
      spec.proofSource match
        case ProofSource.None | ProofSource.OrdinaryProduct => None
        case source                                        => Some(source)
    def canEmitFullTileFitsGuard(domain: LoopDomain, spec: TileSpec): Option[ProofSource] = None

  private final case class NatMulFactProvider(
      factorPolicy: FactorSelectionPolicy
  ) extends TilingFactProvider:
    override def tileSpec(domain: LoopDomain): Option[TileSpec] =
      if domain.dialect != LoopDialect.DAffine then None
      else
        NatProductFacts.selectFactor(domain.upperBound, factorPolicy).map { factor =>
          val tileSize = toIndex(factor.value)
          TileSpec(
            fullUpperBound = domain.upperBound,
            tileSize = tileSize.res,
            prelude = Seq(tileSize),
            proofSource = ProofSource.NatMul,
          )
        }

    override def provePositive(domain: LoopDomain, value: Value[Attribute]): Option[ProofSource] =
      if positive(value) then Some(ProofSource.NatMul) else None

    override def proveExactDivisibility(domain: LoopDomain, spec: TileSpec): Option[ProofSource] =
      if spec.proofSource == ProofSource.NatMul then Some(ProofSource.NatMul) else None

  private object OrdinaryProductFactProvider extends TilingFactProvider:
    override def tileSpec(domain: LoopDomain): Option[TileSpec] =
      domain.upperBound.owner match
        case Some(arith.MulI(_, rhs, _, _))
            if positive(rhs.asInstanceOf[Value[Attribute]]) =>
          Some(TileSpec(domain.upperBound, rhs, Seq.empty, ProofSource.OrdinaryProduct))
        case _ => None

    override def provePositive(domain: LoopDomain, value: Value[Attribute]): Option[ProofSource] =
      if positive(value) then Some(ProofSource.OrdinaryProduct) else None

  private object OrdinaryAffineProductBoundProvider extends TilingFactProvider:
    override def tileSpec(domain: LoopDomain): Option[TileSpec] =
      if domain.dialect != LoopDialect.Affine then None
      else
        domain.upperBound.owner match
          case Some(_: arith.MulI) =>
            Some(TileSpec(domain.upperBound, domain.upperBound, Seq.empty, ProofSource.OrdinaryProduct))
          case _ => None

  private final case class StaticTileFactProvider(tileSize: BigInt) extends TilingFactProvider:
    require(tileSize > 0, s"context tile size must be positive, got $tileSize")

    override def tileSpec(domain: LoopDomain): Option[TileSpec] =
      val tileConst = idxConst(tileSize)
      Some(TileSpec(domain.upperBound, tileConst.result, Seq(tileConst), ProofSource.None))

    override def provePositive(domain: LoopDomain, value: Value[Attribute]): Option[ProofSource] =
      if positive(value) then Some(ProofSource.None) else None

  private object RefinedAssertFactProvider extends TilingFactProvider:
    override def tileSpec(domain: LoopDomain): Option[TileSpec] = None

    override def provePositive(domain: LoopDomain, value: Value[Attribute]): Option[ProofSource] =
      if NatProvenance.isPositive(value) then Some(ProofSource.RefinedAssert) else None

  private object AffineSetFactProvider extends TilingFactProvider:
    override def tileSpec(domain: LoopDomain): Option[TileSpec] = None

    override def canEmitFullTileFitsGuard(domain: LoopDomain, spec: TileSpec): Option[ProofSource] =
      // V1 supports the same normalized 1D condition emitted by separable tiling:
      //   tileIv + tileSize <= upperBound
      domain.dialect match
        case LoopDialect.DAffine if domain.target != TilingTarget.MultiDimBand =>
          Some(ProofSource.AffineSet)
        case _ => None

  private def firstTileSpec(
      domain: LoopDomain,
      providers: Seq[TilingFactProvider],
  ): Option[TileSpec] =
    providers.view.flatMap(_.tileSpec(domain)).headOption

  private def proofQueries(
      domain: LoopDomain,
      spec: TileSpec,
      providers: Seq[TilingFactProvider],
  ): TilingProofs =
    TilingProofs(
      positiveTileSize = providers.view.flatMap(_.provePositive(domain, spec.tileSize)).headOption
        .orElse(if positive(spec.tileSize) then Some(ProofSource.None) else None),
      exactDivisibility = providers.view.flatMap(_.proveExactDivisibility(domain, spec)).headOption,
      fullTileFitsGuard = providers.view.flatMap(_.canEmitFullTileFitsGuard(domain, spec)).headOption,
    )

  private def planFor(
      policy: TilingPolicy,
      target: TilingTarget,
      proofs: TilingProofs,
  ): Option[TilingPlan] =
    if proofs.positiveTileSize.isEmpty then None
    else
      val exactProof = proofs.exactDivisibility
      val decision =
        policy match
          case TilingPolicy.ExactPreferred =>
            if exactProof.nonEmpty then TilingDecision.Exact else TilingDecision.Guarded
          case TilingPolicy.GuardedOnly =>
            TilingDecision.Guarded
          case TilingPolicy.SeparableWhenNotExact =>
            if exactProof.nonEmpty then TilingDecision.Exact
            else if proofs.fullTileFitsGuard.nonEmpty then TilingDecision.Separable
            else TilingDecision.Guarded
      val source =
        exactProof
          .orElse(proofs.fullTileFitsGuard.filter(_ => decision == TilingDecision.Separable))
          .orElse(proofs.positiveTileSize)
          .getOrElse(ProofSource.None)
      Some(TilingPlan(decision, target, source))

  private def legacyPlanFor(policy: TilingPolicy, proofSource: ProofSource, target: TilingTarget): TilingPlan =
    val decision =
      policy match
        case TilingPolicy.ExactPreferred =>
          if proofSource == ProofSource.None then TilingDecision.Guarded else TilingDecision.Exact
        case TilingPolicy.GuardedOnly =>
          TilingDecision.Guarded
        case TilingPolicy.SeparableWhenNotExact =>
          if proofSource == ProofSource.NatMul || proofSource == ProofSource.AffineSet || proofSource == ProofSource.RefinedAssert
          then TilingDecision.Exact
          else TilingDecision.Separable
    TilingPlan(decision, target, proofSource)

  private def asIndex(v: Value[Attribute]): Operand[IndexType] =
    v.asInstanceOf[Operand[IndexType]]

  private def idxConst(v: BigInt): arith.Constant =
    arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

  private def toIndex(nat: Value[Attribute]): DTensor.ShapeToIndex =
    DTensor.ShapeToIndex(
      nat.asInstanceOf[Operand[DTensor.DTensorNatLikeType]],
      Result(IndexType()),
    )

  private def identityMap: AffineMapAttr =
    AffineMapAttr(
      AffineMap(
        dimensions = Seq("d0"),
        symbols = Seq.empty,
        affineExprs = Seq(AffineDimExpr("d0")),
      )
    )

  private def symbolIdentityMap: AffineMapAttr =
    AffineMapAttr(
      AffineMap(
        dimensions = Seq.empty,
        symbols = Seq("s0"),
        affineExprs = Seq(AffineSymExpr("s0")),
      )
    )

  private def shiftedMap(offset: BigInt): AffineMapAttr =
    AffineMapAttr(
      AffineMap(
        dimensions = Seq("d0"),
        symbols = Seq.empty,
        affineExprs = Seq(
          AffineBinaryOpExpr(
            AffineBinaryOp.Add,
            AffineDimExpr("d0"),
            AffineConstantExpr(offset),
          )
        ),
      )
    )

  private def affineTailMap(tileSize: BigInt): AffineMapAttr =
    AffineMapAttr(
      AffineMap(
        dimensions = Seq("d0"),
        symbols = Seq("s0"),
        affineExprs = Seq(
          AffineBinaryOpExpr(
            AffineBinaryOp.Add,
            AffineDimExpr("d0"),
            AffineConstantExpr(tileSize),
          ),
          AffineSymExpr("s0"),
        ),
      )
    )

  private def fullTileFitsSet: AffineSetAttr =
    AffineSetAttr(
      AffineSet(
        dimensions = Seq("d0", "d1"),
        symbols = Seq("s0"),
        affineConstraints = Seq(
          AffineConstraintExpr(
            AffineConstraintKind.LessEqual,
            AffineBinaryOpExpr(
              AffineBinaryOp.Add,
              AffineDimExpr("d0"),
              AffineDimExpr("d1"),
            ),
            AffineSymExpr("s0"),
          )
        ),
      )
    )

  private def isIdentityProjection(map: AffineMapAttr): Boolean =
    map.affineMap.dimensions.size == 1 &&
      map.affineMap.symbols.isEmpty &&
      map.affineMap.affineExprs == Seq(AffineDimExpr(map.affineMap.dimensions.head))

  private def positive(value: Value[Attribute]): Boolean =
    NatProvenance.exactConst(value).exists(_ > 0) || NatProvenance.isPositive(value)

  private def collectDAffineLoops(op: Operation, innermostFirst: Boolean): Seq[d_affine.For] =
    val loops = mutable.ArrayBuffer.empty[d_affine.For]

    def visit(cur: Operation): Unit =
      if innermostFirst then cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
      cur match
        case loop: d_affine.For => loops += loop
        case _                  => ()
      if !innermostFirst then cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

    visit(op)
    loops.toSeq

  private def collectAffineLoops(op: Operation, innermostFirst: Boolean): Seq[affine.For] =
    val loops = mutable.ArrayBuffer.empty[affine.For]

    def visit(cur: Operation): Unit =
      if innermostFirst then cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
      cur match
        case loop: affine.For => loops += loop
        case _                => ()
      if !innermostFirst then cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

    visit(op)
    loops.toSeq

  private def collectExternalValues(block: Block): Seq[Value[Attribute]] =
    val localResults = block.operations.flatMap(_.results).toSet
    val localArgs = block.arguments.toSet
    block.operations
      .flatMap(_.operands)
      .map(_.asInstanceOf[Value[Attribute]])
      .filterNot(v => localArgs.contains(v) || localResults.contains(v))
      .distinct
      .toSeq

  private def cloneBlockBody(
      oldBlock: Block,
      oldIv: Value[Attribute],
      newIv: Value[Attribute],
      oldIterArgs: Seq[Value[Attribute]] = Seq.empty,
      newIterArgs: Seq[Value[Attribute]] = Seq.empty,
  ): Seq[Operation] =
    val blockMapper = mutable.Map.empty[Block, Block]
    val valueMapper = mutable.Map[Value[Attribute], Value[Attribute]](
      oldIv -> newIv
    )
    collectExternalValues(oldBlock).foreach(v => valueMapper.update(v, v))
    oldIterArgs.zip(newIterArgs).foreach { case (oldArg, newArg) =>
      valueMapper.update(oldArg, newArg)
    }

    def mapped[T <: Attribute](v: Value[T]): Value[T] =
      valueMapper.getOrElse(v.asInstanceOf[Value[Attribute]], v.asInstanceOf[Value[Attribute]])
        .asInstanceOf[Value[T]]

    def cloneRegion(region: Region): Region =
      Region(region.blocks.map(cloneBlock))

    def cloneBlock(block: Block): Block =
      val copied = Block.cloneArgumentTypes(block.arguments, Seq.empty)(using valueMapper)
      copied.addOps(block.operations.map(cloneOp).toSeq)
      copied

    def cloneOp(op: Operation): Operation =
      op match
        case loop: affine.For =>
          val copiedResults = loop.res.map(r => Result(r.typ))
          loop.res.zip(copiedResults).foreach { case (oldResult, newResult) =>
            valueMapper.update(oldResult, newResult)
          }
          val copied = affine.For(
            lowerBoundOperands = loop.lowerBoundOperands.map(v => mapped(v).asInstanceOf[Operand[IndexType]]),
            upperBoundOperands = loop.upperBoundOperands.map(v => mapped(v).asInstanceOf[Operand[IndexType]]),
            inits = loop.inits.map(v => mapped(v).asInstanceOf[Operand[Attribute]]),
            res = copiedResults,
            lowerBoundMap = loop.lowerBoundMap,
            upperBoundMap = loop.upperBoundMap,
            step = loop.step,
            body = cloneRegion(loop.body),
          )
          copied.attributes.addAll(loop.attributes)
          copied
        case loop: d_affine.For =>
          val copiedResults = loop.res.map(r => Result(r.typ))
          loop.res.zip(copiedResults).foreach { case (oldResult, newResult) =>
            valueMapper.update(oldResult, newResult)
          }
          val copied = d_affine.For(
            lowerBoundOperands = loop.lowerBoundOperands.map(v => mapped(v).asInstanceOf[Operand[IndexType]]),
            upperBoundOperands = loop.upperBoundOperands.map(v => mapped(v).asInstanceOf[Operand[IndexType]]),
            stepOperands = loop.stepOperands.map(v => mapped(v).asInstanceOf[Operand[IndexType]]),
            inits = loop.inits.map(v => mapped(v).asInstanceOf[Operand[Attribute]]),
            res = copiedResults,
            lowerBoundMap = loop.lowerBoundMap,
            upperBoundMap = loop.upperBoundMap,
            step = loop.step,
            body = cloneRegion(loop.body),
          )
          copied.attributes.addAll(loop.attributes)
          copied
        case ifOp: affine.If =>
          val copiedResults = ifOp.res.map(r => Result(r.typ))
          ifOp.res.zip(copiedResults).foreach { case (oldResult, newResult) =>
            valueMapper.update(oldResult, newResult)
          }
          val copied = affine.If(
            args = ifOp.args.map(v => mapped(v).asInstanceOf[Operand[Attribute]]),
            res = copiedResults,
            condition = ifOp.condition,
            thenRegion = cloneRegion(ifOp.thenRegion),
            elseRegion = cloneRegion(ifOp.elseRegion),
          )
          copied.attributes.addAll(ifOp.attributes)
          copied
        case ifOp: d_affine.If =>
          val copiedResults = ifOp.res.map(r => Result(r.typ))
          ifOp.res.zip(copiedResults).foreach { case (oldResult, newResult) =>
            valueMapper.update(oldResult, newResult)
          }
          val copied = d_affine.If(
            args = ifOp.args.map(v => mapped(v).asInstanceOf[Operand[IndexType]]),
            condition = ifOp.condition,
            thenRegion = cloneRegion(ifOp.thenRegion),
            elseRegion = cloneRegion(ifOp.elseRegion),
            res = copiedResults,
          )
          copied.attributes.addAll(ifOp.attributes)
          copied
        case other =>
          given mutable.Map[Block, Block] = blockMapper
          given mutable.Map[Value[Attribute], Value[Attribute]] = valueMapper
          other.deepCopy

    oldBlock.operations.map(cloneOp).toSeq

  private def staticUnitDAffineLoop(loop: d_affine.For): Boolean =
    loop.stepOperands.isEmpty && loop.step.value.value == 1

  private def eligibleDAffineLoop(loop: d_affine.For, requireReductionLoop: Boolean): Boolean =
    loop.body.blocks.size == 1 &&
      (!requireReductionLoop || (loop.inits.nonEmpty && loop.res.nonEmpty)) &&
      staticUnitDAffineLoop(loop) &&
      loop.lowerBoundOperands.size == 1 &&
      loop.upperBoundOperands.size == 1 &&
      isIdentityProjection(loop.lowerBoundMap) &&
      isIdentityProjection(loop.upperBoundMap) &&
      NatProvenance.exactConst(loop.lowerBoundOperands.head) == Some(0)

  private def eligibleAffineLoop(loop: affine.For, requireReductionLoop: Boolean): Boolean =
    loop.body.blocks.size == 1 &&
      (!requireReductionLoop || (loop.inits.nonEmpty && loop.res.nonEmpty)) &&
      loop.step.value.value == 1 &&
      loop.lowerBoundOperands.size == 1 &&
      loop.upperBoundOperands.size == 1 &&
      isIdentityProjection(loop.lowerBoundMap) &&
      isIdentityProjection(loop.upperBoundMap) &&
      NatProvenance.exactConst(loop.lowerBoundOperands.head) == Some(0)

  private def targetMatches(loop: d_affine.For, target: TilingTarget): Boolean =
    target match
      case TilingTarget.ContextBand =>
        loop.inits.isEmpty && loop.res.isEmpty
      case TilingTarget.ProductReduction =>
        loop.inits.nonEmpty && loop.res.nonEmpty
      case TilingTarget.ExplicitLoop =>
        true
      case TilingTarget.MultiDimBand =>
        false

  private def targetMatches(loop: affine.For, target: TilingTarget): Boolean =
    target match
      case TilingTarget.ContextBand =>
        loop.inits.isEmpty && loop.res.isEmpty
      case TilingTarget.ProductReduction =>
        loop.inits.nonEmpty && loop.res.nonEmpty
      case TilingTarget.ExplicitLoop =>
        true
      case TilingTarget.MultiDimBand =>
        false

  private def normalizeDAffineLoop(
      loop: d_affine.For,
      target: TilingTarget,
  ): Option[LoopDomain] =
    if loop.body.blocks.size != 1 then None
    else if !targetMatches(loop, target) then None
    else if !staticUnitDAffineLoop(loop) then None
    else if loop.lowerBoundOperands.size != 1 || loop.upperBoundOperands.size != 1 then None
    else if !isIdentityProjection(loop.lowerBoundMap) || !isIdentityProjection(loop.upperBoundMap) then None
    else if NatProvenance.exactConst(loop.lowerBoundOperands.head) != Some(0) then None
    else
      Some(
        LoopDomain(
          dialect = LoopDialect.DAffine,
          target = target,
          lowerBound = loop.lowerBoundOperands.head,
          upperBound = loop.upperBoundOperands.head,
          step = loop.step,
          stepOperands = loop.stepOperands,
          hasIterContract = loop.inits.nonEmpty && loop.res.nonEmpty,
          loop = loop,
        )
      )

  private def normalizeAffineLoop(
      loop: affine.For,
      target: TilingTarget,
  ): Option[LoopDomain] =
    if loop.body.blocks.size != 1 then None
    else if !targetMatches(loop, target) then None
    else if loop.step.value.value != 1 then None
    else if loop.lowerBoundOperands.size != 1 || loop.upperBoundOperands.size != 1 then None
    else if !isIdentityProjection(loop.lowerBoundMap) || !isIdentityProjection(loop.upperBoundMap) then None
    else if NatProvenance.exactConst(loop.lowerBoundOperands.head) != Some(0) then None
    else
      Some(
        LoopDomain(
          dialect = LoopDialect.Affine,
          target = target,
          lowerBound = loop.lowerBoundOperands.head,
          upperBound = loop.upperBoundOperands.head,
          step = loop.step,
          stepOperands = Seq.empty,
          hasIterContract = loop.inits.nonEmpty && loop.res.nonEmpty,
          loop = loop,
        )
      )

  private def buildDAffineLoop(
      lowerBound: Value[Attribute],
      upperBound: Value[Attribute],
      step: IntegerAttr,
      stepOperands: Seq[Operand[IndexType]],
      inits: Seq[Operand[Attribute]],
      resultTypes: Seq[Attribute],
      lowerBoundMap: AffineMapAttr = identityMap,
      upperBoundMap: AffineMapAttr = identityMap,
  )(
      bodyBuilder: Seq[Value[Attribute]] => Seq[Operation]
  ): d_affine.For =
    val body = Region(
      Block(
        Seq(IndexType()) ++ inits.map(_.typ),
        args => bodyBuilder(args.toSeq),
      )
    )
    d_affine.For(
      lowerBoundOperands = Seq(asIndex(lowerBound)),
      upperBoundOperands = Seq(asIndex(upperBound)),
      stepOperands = stepOperands,
      inits = inits,
      res = resultTypes.map(Result(_)),
      lowerBoundMap = lowerBoundMap,
      upperBoundMap = upperBoundMap,
      step = step,
      body = body,
    )

  private def builDAffineLoop(
      lowerBound: Value[Attribute],
      upperBounds: Seq[Value[Attribute]],
      step: BigInt,
      inits: Seq[Operand[Attribute]],
      resultTypes: Seq[Attribute],
      lowerBoundMap: AffineMapAttr = identityMap,
      upperBoundMap: AffineMapAttr = identityMap,
  )(
      bodyBuilder: Seq[Value[Attribute]] => Seq[Operation]
  ): affine.For =
    val body = Region(
      Block(
        Seq(IndexType()) ++ inits.map(_.typ),
        args => bodyBuilder(args.toSeq),
      )
    )
    affine.For(
      lowerBoundOperands = Seq(asIndex(lowerBound)),
      upperBoundOperands = upperBounds.map(asIndex),
      inits = inits,
      res = resultTypes.map(Result(_)),
      lowerBoundMap = lowerBoundMap,
      upperBoundMap = upperBoundMap,
      step = IntegerAttr(IntData(step), IndexType()),
      body = body,
    )

  private def emitDAffineInnerLoop(
      oldBlock: Block,
      oldIv: Value[Attribute],
      oldIterArgs: Seq[Value[Attribute]],
      tileIv: Value[Attribute],
      upperBound: Value[Attribute],
      inits: Seq[Operand[Attribute]],
      resultTypes: Seq[Attribute],
      stepAttrType: IntegerType | IndexType,
      upperBoundMap: AffineMapAttr = identityMap,
  ): d_affine.For =
    buildDAffineLoop(
      tileIv,
      upperBound,
      IntegerAttr(IntData(1), stepAttrType),
      Seq.empty,
      inits,
      resultTypes,
      upperBoundMap = upperBoundMap,
    ) { innerArgs =>
      cloneBlockBody(
        oldBlock,
        oldIv,
        innerArgs.head,
        oldIterArgs,
        innerArgs.tail,
      )
    }

  private def emitDAffineTiledLoop(
      loop: d_affine.For,
      spec: TileSpec,
      plan: TilingPlan,
      loopKind: ProductLoopKind,
      useStaticStepForConst: Boolean = true,
      stepAttrType: IntegerType | IndexType = I32,
  ): Boolean =
    if !eligibleDAffineLoop(loop, loopKind == ProductLoopKind.ReductionOnly) then false
    else if !positive(spec.tileSize) then false
    else
      val oldBlock = loop.body.blocks.head
      val oldIv = oldBlock.arguments.head
      val oldIterArgs = oldBlock.arguments.tail.toSeq
      val zero = idxConst(0)
      val staticOne = IntegerAttr(IntData(1), stepAttrType)
      val outerStepConst =
        if useStaticStepForConst then
          NatProvenance.exactConst(spec.tileSize)
            .filter(_ > 0)
            .map(v => IntegerAttr(IntData(v), stepAttrType))
        else None
      val outerStepOperands =
        if outerStepConst.isDefined then Seq.empty
        else Seq(asIndex(spec.tileSize))

      val outerLoop = buildDAffineLoop(
        zero.result,
        spec.fullUpperBound,
        outerStepConst.getOrElse(staticOne),
        outerStepOperands,
        loop.inits.map(_.asInstanceOf[Operand[Attribute]]),
        loop.res.map(_.typ),
      ) { outerArgs =>
        val tileIv = outerArgs.head
        val outerIterArgs = outerArgs.tail
        val staticExactTileSize =
          if plan.decision == TilingDecision.Exact then outerStepConst.map(_.value.value)
          else None

        val (tileEndPrelude, tileEndValue, exactUpperMap) =
          staticExactTileSize match
            case Some(tileSize) =>
              (Seq.empty[Operation], tileIv, shiftedMap(tileSize))
            case None =>
              val tileEnd = arith.AddI(
                tileIv.asInstanceOf[Operand[arith.AnyIntegerType]],
                spec.tileSize.asInstanceOf[Operand[arith.AnyIntegerType]],
                Result(IndexType()),
              )
              (Seq(tileEnd), tileEnd.result, identityMap)

        def exactInner(): d_affine.For =
          emitDAffineInnerLoop(
            oldBlock,
            oldIv,
            oldIterArgs,
            tileIv,
            tileEndValue,
            outerIterArgs.map(_.asInstanceOf[Operand[Attribute]]),
            loop.res.map(_.typ),
            stepAttrType,
            exactUpperMap,
          )

        def guardedInner(): (Seq[Operation], d_affine.For) =
          val clampedTileEnd = arith.MinSI(
            tileEndValue.asInstanceOf[Operand[arith.AnyIntegerType]],
            spec.fullUpperBound.asInstanceOf[Operand[arith.AnyIntegerType]],
            Result(IndexType()),
          )
          val inner = emitDAffineInnerLoop(
            oldBlock,
            oldIv,
            oldIterArgs,
            tileIv,
            clampedTileEnd.result,
            outerIterArgs.map(_.asInstanceOf[Operand[Attribute]]),
            loop.res.map(_.typ),
            stepAttrType,
          )
          (Seq(clampedTileEnd), inner)

        val resultTypes = loop.res.map(_.typ)
        val bodyOps =
          plan.decision match
            case TilingDecision.Exact =>
              val innerLoop = exactInner()
              tileEndPrelude ++ Seq(
                innerLoop,
                d_affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])),
              )
            case TilingDecision.Guarded =>
              val (guardPrelude, innerLoop) = guardedInner()
              tileEndPrelude ++ guardPrelude ++ Seq(
                innerLoop,
                d_affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])),
              )
            case TilingDecision.Separable =>
              val fullInner = exactInner()
              val (guardPrelude, partialInner) = guardedInner()
              val thenRegion = Region(
                Block(
                  Seq.empty,
                  _ => Seq(
                    fullInner,
                    d_affine.Yield(fullInner.results.map(_.asInstanceOf[Operand[Attribute]])),
                  ),
                )
              )
              val elseRegion = Region(
                Block(
                  Seq.empty,
                  _ =>
                    guardPrelude ++ Seq(
                      partialInner,
                      d_affine.Yield(partialInner.results.map(_.asInstanceOf[Operand[Attribute]])),
                    ),
                )
              )
              val ifOp = d_affine.If(
                Seq(asIndex(tileIv), asIndex(spec.tileSize), asIndex(spec.fullUpperBound)),
                fullTileFitsSet,
                thenRegion,
                elseRegion,
                resultTypes.map(Result(_)),
              )
              tileEndPrelude ++ Seq(
                ifOp,
                d_affine.Yield(ifOp.results.map(_.asInstanceOf[Operand[Attribute]])),
              )
        bodyOps
      }

      RewriteMethods.replaceOp(
        loop,
        zero +: (spec.prelude :+ outerLoop),
        Some(outerLoop.results),
      )
      true

  private def emitAffineGuardedProductLoop(
      loop: affine.For,
      tileSize: BigInt,
      requireReductionLoop: Boolean,
  ): Boolean =
    val target =
      if requireReductionLoop then TilingTarget.ProductReduction else TilingTarget.ExplicitLoop
    if !eligibleAffineLoop(loop, requireReductionLoop) then false
    else
      val specOpt =
        normalizeAffineLoop(loop, target).flatMap(domain =>
          OrdinaryAffineProductBoundProvider.tileSpec(domain)
        )
      specOpt.map(_.fullUpperBound) match
        case None => false
        case Some(fullUpperBound) =>
          val oldBlock = loop.body.blocks.head
          val oldIv = oldBlock.arguments.head
          val oldIterArgs = oldBlock.arguments.tail.toSeq
          val zero = idxConst(0)

          val outerLoop = builDAffineLoop(
            zero.result,
            Seq(fullUpperBound),
            tileSize,
            loop.inits.map(_.asInstanceOf[Operand[Attribute]]),
            loop.res.map(_.typ),
            upperBoundMap = symbolIdentityMap,
          ) { outerArgs =>
            val tileIv = outerArgs.head
            val outerIterArgs = outerArgs.tail
            val innerLoop = builDAffineLoop(
              tileIv,
              Seq(tileIv, fullUpperBound),
              1,
              outerIterArgs.map(_.asInstanceOf[Operand[Attribute]]),
              loop.res.map(_.typ),
              upperBoundMap = affineTailMap(tileSize),
            ) { innerArgs =>
              cloneBlockBody(oldBlock, oldIv, innerArgs.head, oldIterArgs, innerArgs.tail)
            }
            Seq(
              innerLoop,
              affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])),
            )
          }

          RewriteMethods.replaceOp(loop, Seq(zero, outerLoop), Some(outerLoop.results))
          true

  private def emitAffineContextLoop(loop: affine.For, tileSize: BigInt): Boolean =
    if !eligibleAffineLoop(loop, requireReductionLoop = false) || loop.inits.nonEmpty || loop.res.nonEmpty
    then false
    else
      val oldBlock = loop.body.blocks.head
      val oldIv = oldBlock.arguments.head
      val outerLoop = builDAffineLoop(
        loop.lowerBoundOperands.head,
        loop.upperBoundOperands.map(_.asInstanceOf[Value[Attribute]]),
        tileSize,
        Seq.empty,
        Seq.empty,
        lowerBoundMap = loop.lowerBoundMap,
        upperBoundMap = loop.upperBoundMap,
      ) { outerArgs =>
        val tileIv = outerArgs.head
        val innerLoop = builDAffineLoop(
          tileIv,
          Seq(tileIv, loop.upperBoundOperands.head),
          1,
          Seq.empty,
          Seq.empty,
          upperBoundMap = affineTailMap(tileSize),
        ) { innerArgs =>
          cloneBlockBody(oldBlock, oldIv, innerArgs.head)
        }
        Seq(innerLoop, affine.Yield(Seq.empty))
      }
      RewriteMethods.replaceOp(loop, Seq(outerLoop), None)
      true

  private def emitDAffineContextLoop(
      loop: d_affine.For,
      spec: TileSpec,
      plan: TilingPlan,
      useStaticStepForConst: Boolean = true,
      stepAttrType: IntegerType | IndexType = IndexType(),
  ): Boolean =
    if !eligibleDAffineLoop(loop, requireReductionLoop = false) || loop.inits.nonEmpty || loop.res.nonEmpty
    then false
    else
      emitDAffineTiledLoop(
        loop,
        spec,
        plan,
        ProductLoopKind.AnyProductLoop,
        useStaticStepForConst = useStaticStepForConst,
        stepAttrType = stepAttrType,
      )

  def transformDAffineNatmulProduct(
      op: Operation,
      mode: TailMode,
      factorPolicy: FactorSelectionPolicy,
      loopKind: ProductLoopKind,
  ): Operation =
    var changed = true
    while changed do
      changed = false
      collectDAffineLoops(op, innermostFirst = true).foreach { loop =>
        if loop.containerBlock.nonEmpty then
          val target = loopKindToTarget(loopKind)
          val specOpt =
            normalizeDAffineLoop(loop, target).flatMap(domain =>
              firstTileSpec(domain, Seq(NatMulFactProvider(factorPolicy)))
            )
          specOpt.foreach { spec =>
            val decision =
              mode match
                case TailMode.Exact     => TilingDecision.Exact
                case TailMode.Guarded   => TilingDecision.Guarded
                case TailMode.Separable => TilingDecision.Separable
            if emitDAffineTiledLoop(loop, spec, TilingPlan(decision, target, spec.proofSource), loopKind) then changed = true
          }
      }
    op

  def transformDAffineOrdinaryProduct(
      op: Operation,
      loopKind: ProductLoopKind,
  ): Operation =
    var changed = true
    while changed do
      changed = false
      collectDAffineLoops(op, innermostFirst = true).foreach { loop =>
        if loop.containerBlock.nonEmpty then
          val target = loopKindToTarget(loopKind)
          val specOpt =
            normalizeDAffineLoop(loop, target).flatMap(domain =>
              firstTileSpec(domain, Seq(OrdinaryProductFactProvider))
            )
          specOpt.foreach { spec =>
            if emitDAffineTiledLoop(loop, spec, TilingPlan(TilingDecision.Guarded, target, spec.proofSource), loopKind) then
              changed = true
          }
      }
    op

  private def loopKindToTarget(loopKind: ProductLoopKind): TilingTarget =
    loopKind match
      case ProductLoopKind.ReductionOnly => TilingTarget.ProductReduction
      case ProductLoopKind.AnyProductLoop => TilingTarget.ExplicitLoop

  def transformDAffineProductByPolicy(
      op: Operation,
      policy: TilingPolicy,
      factorPolicy: FactorSelectionPolicy,
      loopKind: ProductLoopKind,
  ): Operation =
    var changed = true
    while changed do
      changed = false
      collectDAffineLoops(op, innermostFirst = true).foreach { loop =>
        if loop.containerBlock.nonEmpty then
          val target = loopKindToTarget(loopKind)
          val providers = Seq(
            NatMulFactProvider(factorPolicy),
            OrdinaryProductFactProvider,
            RefinedAssertFactProvider,
            AffineSetFactProvider,
          )
          val specOpt =
            normalizeDAffineLoop(loop, target).flatMap(domain =>
              firstTileSpec(domain, providers).map(domain -> _)
            )
          specOpt.foreach { case (domain, spec) =>
            planFor(policy, target, proofQueries(domain, spec, providers)).foreach { plan =>
              if emitDAffineTiledLoop(loop, spec, plan, loopKind) then changed = true
            }
          }
      }
    op

  def transformAffineProductGuarded(
      op: Operation,
      tileSize: BigInt,
      requireReductionLoop: Boolean,
  ): Operation =
    require(tileSize > 0, s"ordinary affine tile size must be positive, got $tileSize")
    var changed = true
    while changed do
      changed = false
      collectAffineLoops(op, innermostFirst = true).foreach { loop =>
        if loop.containerBlock.nonEmpty && emitAffineGuardedProductLoop(loop, tileSize, requireReductionLoop)
        then changed = true
      }
    op

  def transformAffineContextGuarded(op: Operation, tileSize: BigInt): Operation =
    require(tileSize > 0, s"context tile size must be positive, got $tileSize")
    var changed = true
    while changed do
      changed = false
      collectAffineLoops(op, innermostFirst = false).foreach { loop =>
        if loop.containerBlock.nonEmpty && emitAffineContextLoop(loop, tileSize) then changed = true
      }
    op

  def transformDAffineContextGuarded(op: Operation, tileSize: BigInt): Operation =
    require(tileSize > 0, s"context tile size must be positive, got $tileSize")
    var changed = true
    while changed do
      changed = false
      collectDAffineLoops(op, innermostFirst = false).foreach { loop =>
        if loop.containerBlock.nonEmpty then
          val specOpt =
            normalizeDAffineLoop(loop, TilingTarget.ContextBand).flatMap(domain =>
              firstTileSpec(domain, Seq(StaticTileFactProvider(tileSize)))
            )
          specOpt.foreach { spec =>
            if emitDAffineContextLoop(
                loop,
                spec,
                TilingPlan(TilingDecision.Guarded, TilingTarget.ContextBand, spec.proofSource),
                stepAttrType = IndexType(),
              )
            then changed = true
          }
      }
    op

  def transformDAffineContextNatmul(
      op: Operation,
      factorPolicy: FactorSelectionPolicy,
      mode: TailMode,
  ): Operation =
    var changed = true
    while changed do
      changed = false
      collectDAffineLoops(op, innermostFirst = false).foreach { loop =>
        if loop.containerBlock.nonEmpty then
          val specOpt =
            normalizeDAffineLoop(loop, TilingTarget.ContextBand).flatMap(domain =>
              firstTileSpec(domain, Seq(NatMulFactProvider(factorPolicy)))
            )
          specOpt.foreach { spec =>
            val decision =
              mode match
                case TailMode.Exact     => TilingDecision.Exact
                case TailMode.Guarded   => TilingDecision.Guarded
                case TailMode.Separable => TilingDecision.Separable
            if emitDAffineContextLoop(
                loop,
                spec,
                TilingPlan(decision, TilingTarget.ContextBand, spec.proofSource),
                useStaticStepForConst = false,
                stepAttrType = IndexType(),
              )
            then changed = true
          }
      }
    op
