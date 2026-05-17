package scair.passes.context_band_tiling

import scair.MLContext
import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.analysis.NatProductFacts
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

import scala.collection.mutable

final class OrdinaryAffineContextBandTileWithTail(ctx: MLContext, tileSize: BigInt)
    extends ModulePass(ctx):
  override val name: String = "ordinary-affine-context-band-tile-with-tail"

  override def transform(op: Operation): Operation =
    ContextBandTileWithTailTransform.transformAffine(op, tileSize)

final class DependentContextBandTileWithTail(ctx: MLContext, tileSize: BigInt)
    extends ModulePass(ctx):
  override val name: String = "dependent-context-band-tile-with-tail"

  override def transform(op: Operation): Operation =
    ContextBandTileWithTailTransform.transformDAffine(op, tileSize)

final class DependentContextBandExactTile(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "dependent-context-band-exact-tile"

  override def transform(op: Operation): Operation =
    ContextBandTileWithTailTransform.transformDAffineExactNatmul(op)

final class DependentContextBandFactorTileWithTail(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "dependent-context-band-factor-tile-with-tail"

  override def transform(op: Operation): Operation =
    ContextBandTileWithTailTransform.transformDAffineFactorGuardedNatmul(op)

private object ContextBandTileWithTailTransform:
  private val ordinaryMarker = "scair.context_band_tile_with_tail.generated"
  private val ordinaryMode = "scair.context_band_tile_with_tail.mode"
  private val dependentMarker = "scair.dependent_context_band_tile_with_tail.generated"
  private val dependentMode = "scair.dependent_context_band_tile_with_tail.mode"

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

  private def tileTailMap(tileSize: BigInt): AffineMapAttr =
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

  private def isIdentityProjection(map: AffineMapAttr): Boolean =
    map.affineMap.dimensions.size == 1 &&
      map.affineMap.symbols.isEmpty &&
      map.affineMap.affineExprs == Seq(AffineDimExpr(map.affineMap.dimensions.head))

  private def hasAnyTilingMarker(op: Operation): Boolean =
    op.attributes.contains(ordinaryMarker) ||
      op.attributes.contains(ordinaryMode) ||
      op.attributes.contains(dependentMarker) ||
      op.attributes.contains(dependentMode) ||
      hasProductTilingMarker(op)

  private def hasProductTilingMarker(op: Operation): Boolean =
    op.attributes.contains("scair.ordinary_affine_product_tile_with_tail.generated") ||
      op.attributes.contains("scair.ordinary_affine_product_tile_with_tail.mode") ||
      op.attributes.contains("scair.dependent_exact_tile.generated") ||
      op.attributes.contains("scair.dependent_exact_tile.mode") ||
      op.attributes.contains("scair.dependent_tile_with_tail_control.generated") ||
      op.attributes.contains("scair.dependent_tile_with_tail_control.mode") ||
      op.attributes.contains("scair.ordinary_product_tile_with_tail.generated") ||
      op.attributes.contains("scair.ordinary_product_tile_with_tail.mode") ||
      op.attributes.contains("scair.dependent_natmul_factorization.generated") ||
      op.attributes.contains("scair.dependent_natmul_factorization.mode")

  private def hasProductMarkedAncestor(op: Operation): Boolean =
    op.containerBlock
      .flatMap(_.containerRegion)
      .flatMap(_.containerOperation)
      .exists(parent => hasProductTilingMarker(parent) || hasProductMarkedAncestor(parent))

  private def collectAffineLoopsOutermostFirst(op: Operation): Seq[affine.For] =
    val loops = mutable.ArrayBuffer.empty[affine.For]

    def visit(cur: Operation): Unit =
      cur match
        case loop: affine.For => loops += loop
        case _                => ()
      cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

    visit(op)
    loops.toSeq

  private def collectDAffineLoopsOutermostFirst(op: Operation): Seq[d_affine.For] =
    val loops = mutable.ArrayBuffer.empty[d_affine.For]

    def visit(cur: Operation): Unit =
      cur match
        case loop: d_affine.For => loops += loop
        case _                  => ()
      cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

    visit(op)
    loops.toSeq

  private def eligibleAffine(loop: affine.For): Boolean =
    !hasAnyTilingMarker(loop) &&
      !hasProductMarkedAncestor(loop) &&
      loop.inits.isEmpty &&
      loop.res.isEmpty &&
      loop.step.value.value == 1 &&
      loop.body.blocks.size == 1 &&
      loop.lowerBoundOperands.size == 1 &&
      loop.upperBoundOperands.size == 1 &&
      isIdentityProjection(loop.lowerBoundMap) &&
      isIdentityProjection(loop.upperBoundMap)

  private def eligibleDAffine(loop: d_affine.For): Boolean =
    !hasAnyTilingMarker(loop) &&
      !hasProductMarkedAncestor(loop) &&
      loop.inits.isEmpty &&
      loop.res.isEmpty &&
      loop.stepOperands.isEmpty &&
      loop.step.value.value == 1 &&
      loop.body.blocks.size == 1 &&
      loop.lowerBoundOperands.size == 1 &&
      loop.upperBoundOperands.size == 1 &&
      isIdentityProjection(loop.lowerBoundMap) &&
      isIdentityProjection(loop.upperBoundMap)

  private def buildAffineLoop(
      lowerBound: Value[Attribute],
      upperBounds: Seq[Value[Attribute]],
      step: BigInt,
      lowerBoundMap: AffineMapAttr = identityMap,
      upperBoundMap: AffineMapAttr = identityMap,
  )(
      bodyBuilder: Value[Attribute] => Seq[Operation]
  ): affine.For =
    affine.For(
      lowerBoundOperands = Seq(asIndex(lowerBound)),
      upperBoundOperands = upperBounds.map(asIndex),
      inits = Seq.empty,
      res = Seq.empty,
      lowerBoundMap = lowerBoundMap,
      upperBoundMap = upperBoundMap,
      step = IntegerAttr(IntData(step), IndexType()),
      body = Region(
        Block(
          Seq(IndexType()),
          args => bodyBuilder(args.head),
        )
      ),
    )

  private def buildDAffineLoop(
      lowerBound: Value[Attribute],
      upperBound: Value[Attribute],
      step: BigInt,
      stepOperands: Seq[Operand[IndexType]] = Seq.empty,
  )(
      bodyBuilder: Value[Attribute] => Seq[Operation]
  ): d_affine.For =
    d_affine.For(
      lowerBoundOperands = Seq(asIndex(lowerBound)),
      upperBoundOperands = Seq(asIndex(upperBound)),
      stepOperands = stepOperands,
      inits = Seq.empty,
      res = Seq.empty,
      lowerBoundMap = identityMap,
      upperBoundMap = identityMap,
      step = IntegerAttr(IntData(step), IndexType()),
      body = Region(
        Block(
          Seq(IndexType()),
          args => bodyBuilder(args.head),
        )
      ),
    )

  private def cloneAffineBody(oldBlock: Block, oldIv: Value[Attribute], newIv: Value[Attribute]): Seq[Operation] =
    val valueMapper = mutable.Map[Value[Attribute], Value[Attribute]](oldIv -> newIv)

    def mapped[T <: Attribute](v: Value[T]): Value[T] =
      valueMapper.getOrElse(v, v).asInstanceOf[Value[T]]

    def cloneBlock(block: Block): Block =
      Block(
        block.arguments.map(_.typ),
        args =>
          block.arguments.zip(args).foreach { case (oldArg, newArg) =>
            valueMapper.update(oldArg, newArg)
          }
          block.operations.map(cloneOp).toSeq,
      )

    def cloneRegion(region: Region): Region =
      Region(region.blocks.map(cloneBlock))

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
        case other =>
          given mutable.Map[Block, Block] = mutable.Map.empty
          given mutable.Map[Value[Attribute], Value[Attribute]] = valueMapper
          other.deepCopy

    oldBlock.operations.map(cloneOp).toSeq

  private def tryTileAffine(loop: affine.For, tileSize: BigInt): Boolean =
    if !eligibleAffine(loop) then false
    else
      val oldBlock = loop.body.blocks.head
      val oldIv = oldBlock.arguments.head

      val outerLoop = buildAffineLoop(
        loop.lowerBoundOperands.head,
        loop.upperBoundOperands.map(_.asInstanceOf[Value[Attribute]]),
        tileSize,
        lowerBoundMap = loop.lowerBoundMap,
        upperBoundMap = loop.upperBoundMap,
      ) { tileIv =>
        val innerLoop = buildAffineLoop(
          tileIv,
          Seq(tileIv, loop.upperBoundOperands.head),
          1,
          upperBoundMap = tileTailMap(tileSize),
        ) { innerIv =>
          cloneAffineBody(oldBlock, oldIv, innerIv)
        }
        innerLoop.attributes.addOne(ordinaryMarker -> StringData("inner"))
        Seq(innerLoop, affine.Yield(Seq.empty))
      }

      outerLoop.attributes.addOne(ordinaryMode -> StringData("static_step_tail_guarded_context"))
      outerLoop.attributes.addOne(ordinaryMarker -> StringData("outer"))

      RewriteMethods.replaceOp(loop, Seq(outerLoop), None)
      true

  private def tryTileDAffine(loop: d_affine.For, tileSize: BigInt): Boolean =
    if !eligibleDAffine(loop) then false
    else
      val oldBlock = loop.body.blocks.head
      val oldIv = oldBlock.arguments.head
      val tileSizeConst = idxConst(tileSize)

      val outerLoop = buildDAffineLoop(
        loop.lowerBoundOperands.head,
        loop.upperBoundOperands.head,
        tileSize,
      ) { tileIv =>
        val tileEnd = arith.AddI(
          tileIv.asInstanceOf[Operand[arith.AnyIntegerType]],
          tileSizeConst.result.asInstanceOf[Operand[arith.AnyIntegerType]],
          Result(IndexType()),
        )
        val clampedTileEnd = arith.MinSI(
          tileEnd.result.asInstanceOf[Operand[arith.AnyIntegerType]],
          loop.upperBoundOperands.head.asInstanceOf[Operand[arith.AnyIntegerType]],
          Result(IndexType()),
        )
        val innerLoop = buildDAffineLoop(
          tileIv,
          clampedTileEnd.result,
          1,
        ) { innerIv =>
          cloneAffineBody(oldBlock, oldIv, innerIv)
        }
        innerLoop.attributes.addOne(dependentMarker -> StringData("inner"))
        Seq(
          tileEnd,
          clampedTileEnd,
          innerLoop,
          d_affine.Yield(Seq.empty),
        )
      }

      outerLoop.attributes.addOne(dependentMode -> StringData("static_step_tail_guarded_context"))
      outerLoop.attributes.addOne(dependentMarker -> StringData("outer"))

      RewriteMethods.replaceOp(loop, Seq(tileSizeConst, outerLoop), None)
      true

  private def natmulRhsTileSize(loop: d_affine.For): Option[Value[Attribute]] =
    if loop.upperBoundOperands.size != 1 then None
    else
      NatProductFacts.rightmostPositiveFactor(loop.upperBoundOperands.head).map(_.value)

  private def tryTileDAffineExactNatmul(loop: d_affine.For): Boolean =
    if !eligibleDAffine(loop) then false
    else
      natmulRhsTileSize(loop) match
        case None => false
        case Some(tileNat) =>
          val oldBlock = loop.body.blocks.head
          val oldIv = oldBlock.arguments.head
          val zero = idxConst(0)
          val tileSize = toIndex(tileNat)
          val staticOne = BigInt(1)

          val outerLoop = buildDAffineLoop(
            zero.result,
            loop.upperBoundOperands.head,
            staticOne,
            stepOperands = Seq(asIndex(tileSize.res)),
          ) { tileIv =>
            val tileEnd = arith.AddI(
              tileIv.asInstanceOf[Operand[arith.AnyIntegerType]],
              tileSize.res.asInstanceOf[Operand[arith.AnyIntegerType]],
              Result(IndexType()),
            )
            val innerLoop = buildDAffineLoop(
              tileIv,
              tileEnd.result,
              staticOne,
            ) { innerIv =>
              cloneAffineBody(oldBlock, oldIv, innerIv)
            }
            innerLoop.attributes.addOne(dependentMarker -> StringData("inner_exact"))
            Seq(
              tileEnd,
              innerLoop,
              d_affine.Yield(Seq.empty),
            )
          }

          outerLoop.attributes.addOne(dependentMode -> StringData("dynamic_step_tail_free_exact_context"))
          outerLoop.attributes.addOne(dependentMarker -> StringData("outer_exact"))

          RewriteMethods.replaceOp(loop, Seq(zero, tileSize, outerLoop), None)
          true

  private def tryTileDAffineFactorGuardedNatmul(loop: d_affine.For): Boolean =
    if !eligibleDAffine(loop) then false
    else
      natmulRhsTileSize(loop) match
        case None => false
        case Some(tileNat) =>
          val oldBlock = loop.body.blocks.head
          val oldIv = oldBlock.arguments.head
          val zero = idxConst(0)
          val tileSize = toIndex(tileNat)
          val staticOne = BigInt(1)

          val outerLoop = buildDAffineLoop(
            zero.result,
            loop.upperBoundOperands.head,
            staticOne,
            stepOperands = Seq(asIndex(tileSize.res)),
          ) { tileIv =>
            val tileEnd = arith.AddI(
              tileIv.asInstanceOf[Operand[arith.AnyIntegerType]],
              tileSize.res.asInstanceOf[Operand[arith.AnyIntegerType]],
              Result(IndexType()),
            )
            val clampedTileEnd = arith.MinSI(
              tileEnd.result.asInstanceOf[Operand[arith.AnyIntegerType]],
              loop.upperBoundOperands.head.asInstanceOf[Operand[arith.AnyIntegerType]],
              Result(IndexType()),
            )
            val innerLoop = buildDAffineLoop(
              tileIv,
              clampedTileEnd.result,
              staticOne,
            ) { innerIv =>
              cloneAffineBody(oldBlock, oldIv, innerIv)
            }
            innerLoop.attributes.addOne(dependentMarker -> StringData("inner_factor_guarded"))
            Seq(
              tileEnd,
              clampedTileEnd,
              innerLoop,
              d_affine.Yield(Seq.empty),
            )
          }

          outerLoop.attributes.addOne(dependentMode -> StringData("dynamic_step_tail_guarded_context"))
          outerLoop.attributes.addOne(dependentMarker -> StringData("outer_factor_guarded"))

          RewriteMethods.replaceOp(loop, Seq(zero, tileSize, outerLoop), None)
          true

  def transformAffine(op: Operation, tileSize: BigInt): Operation =
    require(tileSize > 0, s"context tile size must be positive, got $tileSize")
    var changed = true
    while changed do
      changed = false
      collectAffineLoopsOutermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryTileAffine(loop, tileSize) then changed = true
      }
    op

  def transformDAffine(op: Operation, tileSize: BigInt): Operation =
    require(tileSize > 0, s"context tile size must be positive, got $tileSize")
    var changed = true
    while changed do
      changed = false
      collectDAffineLoopsOutermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryTileDAffine(loop, tileSize) then changed = true
      }
    op

  def transformDAffineExactNatmul(op: Operation): Operation =
    var changed = true
    while changed do
      changed = false
      collectDAffineLoopsOutermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryTileDAffineExactNatmul(loop) then changed = true
      }
    op

  def transformDAffineFactorGuardedNatmul(op: Operation): Operation =
    var changed = true
    while changed do
      changed = false
      collectDAffineLoopsOutermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryTileDAffineFactorGuardedNatmul(loop) then changed = true
      }
    op
