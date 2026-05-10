package scair.passes.dependent_natmul_tiling

import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.NatProvenance
import scair.passes.analysis.NatProductFacts
import scair.transformations.RewriteMethods

import scala.collection.mutable

private[dependent_natmul_tiling] enum TailPolicy:
  case Exact
  case Guarded

private[dependent_natmul_tiling] enum ProductLoopKind:
  case ReductionOnly
  case AnyProductLoop

private[dependent_natmul_tiling] final case class DependentNatmulTilingAttributes(
    modeKey: String,
    mode: String,
    generatedKey: String,
    tailFreeKey: String,
    tailFree: String,
    proofKey: String,
    proof: String,
)

private[dependent_natmul_tiling] object DependentNatmulTilingTransform:
  def transform(
      op: Operation,
      tailPolicy: TailPolicy,
      attributes: DependentNatmulTilingAttributes,
      loopKind: ProductLoopKind = ProductLoopKind.ReductionOnly,
  ): Operation =
    transformWithPlan(op, tailPolicy, attributes, chooseNatmulPlan, loopKind)

  def transformOrdinaryIndexProduct(
      op: Operation,
      attributes: DependentNatmulTilingAttributes,
      loopKind: ProductLoopKind = ProductLoopKind.ReductionOnly,
  ): Operation =
    transformWithPlan(op, TailPolicy.Guarded, attributes, chooseOrdinaryIndexProductPlan, loopKind)

  private def transformWithPlan(
      op: Operation,
      tailPolicy: TailPolicy,
      attributes: DependentNatmulTilingAttributes,
      choosePlan: d_affine.For => Option[TilePlan],
      loopKind: ProductLoopKind,
  ): Operation =
    var changed = true
    while changed do
      changed = false
      collectLoopsInnermostFirst(op).foreach {
        case loop: d_affine.For =>
          if loop.containerBlock.nonEmpty && tryTile(loop, tailPolicy, attributes, choosePlan, loopKind) then
            changed = true
      }
    op

  private def asIndex(v: Value[Attribute]): Operand[IndexType] =
    v.asInstanceOf[Operand[IndexType]]

  private def idxConst(v: BigInt): arith.Constant =
    arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

  private def toIndex(nat: Value[Attribute]): dTensor.ShapeToIndex =
    dTensor.ShapeToIndex(
      nat.asInstanceOf[Operand[dTensor.dTensorNatLikeType]],
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

  private def isIdentityProjection(map: AffineMapAttr): Boolean =
    map.affineMap.dimensions.size == 1 &&
      map.affineMap.symbols.isEmpty &&
      map.affineMap.affineExprs == Seq(AffineDimExpr(map.affineMap.dimensions.head))

  private def collectLoopsInnermostFirst(op: Operation): Seq[d_affine.For] =
    val loops = mutable.ArrayBuffer.empty[d_affine.For]

    def visit(cur: Operation): Unit =
      cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
      cur match
        case loop: d_affine.For => loops += loop
        case _                  => ()

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

  private final case class TilePlan(
      prelude: Seq[Operation],
      fullUpperBound: Value[Attribute],
      tileSize: Value[Attribute],
  )

  private def chooseNatmulPlan(loop: d_affine.For): Option[TilePlan] =
    if loop.upperBoundOperands.size != 1 then None
    else
      NatProductFacts.rightmostPositiveFactor(loop.upperBoundOperands.head).map { factor =>
        val tileSize = toIndex(factor.value)
        TilePlan(
          prelude = Seq(tileSize),
          fullUpperBound = loop.upperBoundOperands.head,
          tileSize = tileSize.res,
        )
      }

  private def chooseOrdinaryIndexProductPlan(loop: d_affine.For): Option[TilePlan] =
    if loop.upperBoundOperands.size != 1 then None
    else
      loop.upperBoundOperands.head.owner match
        case Some(arith.MulI(_, rhs, _, _))
            if NatProvenance.exactConst(rhs).exists(_ > 0) || NatProvenance.isPositive(rhs) =>
          Some(
            TilePlan(
              prelude = Seq.empty,
              fullUpperBound = loop.upperBoundOperands.head,
              tileSize = rhs,
            )
          )
        case _ => None

  private def buildLoop(
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
      res = resultTypes.map(ty => Result(ty)),
      lowerBoundMap = lowerBoundMap,
      upperBoundMap = upperBoundMap,
      step = step,
      body = body,
    )

  private def hasTileMarker(loop: d_affine.For): Boolean =
    loop.attributes.contains("scair.dependent_exact_tile.mode") ||
      loop.attributes.contains("scair.dependent_exact_tile.generated") ||
      loop.attributes.contains("scair.dependent_product_loop_exact_tile.mode") ||
      loop.attributes.contains("scair.dependent_product_loop_exact_tile.generated") ||
      loop.attributes.contains("scair.dependent_tile_with_tail_control.mode") ||
      loop.attributes.contains("scair.dependent_tile_with_tail_control.generated") ||
      loop.attributes.contains("scair.ordinary_product_tile_with_tail.mode") ||
      loop.attributes.contains("scair.ordinary_product_tile_with_tail.generated") ||
      loop.attributes.contains("scair.attention.tile.mode") ||
      loop.attributes.contains("scair.attention.tile.generated")

  private def isStaticUnitStep(loop: d_affine.For): Boolean =
    loop.stepOperands.isEmpty && loop.step.value.value == 1

  private def tryTile(
      loop: d_affine.For,
      tailPolicy: TailPolicy,
      attributes: DependentNatmulTilingAttributes,
      choosePlan: d_affine.For => Option[TilePlan],
      loopKind: ProductLoopKind,
  ): Boolean =
    if hasTileMarker(loop) then false
    else if loop.body.blocks.size != 1 then false
    else if loopKind == ProductLoopKind.ReductionOnly && (loop.inits.isEmpty || loop.res.isEmpty) then false
    else if !isStaticUnitStep(loop) then false
    else if loop.lowerBoundOperands.size != 1 || loop.upperBoundOperands.size != 1 then false
    else if !isIdentityProjection(loop.lowerBoundMap) || !isIdentityProjection(loop.upperBoundMap) then false
    else if NatProvenance.exactConst(loop.lowerBoundOperands.head) != Some(0) then false
    else
      choosePlan(loop) match
        case None => false
        case Some(plan) =>
          if !NatProvenance.exactConst(plan.tileSize).exists(_ > 0) &&
              !NatProvenance.isPositive(plan.tileSize)
          then false
          else {
          val oldBlock = loop.body.blocks.head
          val oldIv = oldBlock.arguments.head
          val oldIterArgs = oldBlock.arguments.tail
          val externalValues = collectExternalValues(oldBlock)
          val zero = idxConst(0)
          val staticOne = IntegerAttr(IntData(1), I32)
          val outerStepConst = NatProvenance.exactConst(plan.tileSize)
            .filter(_ > 0)
            .map(v => IntegerAttr(IntData(v), I32))
          val outerStepOperands =
            if outerStepConst.isDefined then Seq.empty
            else Seq(asIndex(plan.tileSize))

          val outerLoop = buildLoop(
            zero.result,
            plan.fullUpperBound,
            outerStepConst.getOrElse(staticOne),
            outerStepOperands,
            loop.inits.map(_.asInstanceOf[Operand[Attribute]]),
            loop.res.map(_.typ),
          ) { outerArgs =>
            val tileIv = outerArgs.head
            val outerIterArgs = outerArgs.tail
            val staticExactTileSize =
              if tailPolicy == TailPolicy.Exact then outerStepConst.map(_.value.value)
              else None
            val (tileEndPrelude, tileEndValue, tileEndMap) =
              staticExactTileSize match
                case Some(tileSize) =>
                  (Seq.empty[Operation], tileIv, shiftedMap(tileSize))
                case None =>
                  val tileEnd = arith.AddI(
                    tileIv.asInstanceOf[Operand[arith.AnyIntegerType]],
                    plan.tileSize.asInstanceOf[Operand[arith.AnyIntegerType]],
                    Result(IndexType()),
                  )
                  (Seq(tileEnd), tileEnd.result, identityMap)

            val (boundPrelude, innerUpperBound, innerUpperBoundMap) =
              tailPolicy match
                case TailPolicy.Exact =>
                  (Seq.empty[Operation], tileEndValue, tileEndMap)
                case TailPolicy.Guarded =>
                  val clampedTileEnd = arith.MinSI(
                    tileEndValue.asInstanceOf[Operand[arith.AnyIntegerType]],
                    plan.fullUpperBound.asInstanceOf[Operand[arith.AnyIntegerType]],
                    Result(IndexType()),
                  )
                  (Seq(clampedTileEnd), clampedTileEnd.result, identityMap)

            val innerLoop = buildLoop(
              tileIv,
              innerUpperBound,
              staticOne,
              Seq.empty,
              outerIterArgs.map(_.asInstanceOf[Operand[Attribute]]),
              loop.res.map(_.typ),
              upperBoundMap = innerUpperBoundMap,
            ) { innerArgs =>
              val innerIv = innerArgs.head
              val innerIterArgs = innerArgs.tail
              given mutable.Map[Block, Block] = mutable.Map.empty
              val valueMapper = mutable.Map[Value[Attribute], Value[Attribute]](
                oldIv -> innerIv
              )
              externalValues.foreach(v => valueMapper.update(v, v))
              oldIterArgs.zip(innerIterArgs).foreach { case (oldArg, newArg) =>
                valueMapper.update(oldArg, newArg)
              }
              given mutable.Map[Value[Attribute], Value[Attribute]] = valueMapper
              oldBlock.operations.map(_.deepCopy).toSeq
            }
            innerLoop.attributes.addOne(attributes.generatedKey -> StringData("inner"))

            tileEndPrelude ++ boundPrelude ++ Seq(
              innerLoop,
              d_affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])),
            )
          }

          outerLoop.attributes.addOne(attributes.modeKey -> StringData(attributes.mode))
          outerLoop.attributes.addOne(attributes.tailFreeKey -> StringData(attributes.tailFree))
          outerLoop.attributes.addOne(attributes.proofKey -> StringData(attributes.proof))
          outerLoop.attributes.addOne(attributes.generatedKey -> StringData("outer"))

          RewriteMethods.replaceOp(
            loop,
            zero +: (plan.prelude :+ outerLoop),
            Some(outerLoop.results),
          )
          true
          }
