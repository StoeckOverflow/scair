package scair.passes.dependent_natmul_tiling

import scair.MLContext
import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.ir.*
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

import scala.collection.mutable

final class OrdinaryAffineProductTileWithTail(ctx: MLContext, tileSize: BigInt) extends ModulePass(ctx):
  override val name: String = "ordinary-affine-product-tile-with-tail"

  override def transform(op: Operation): Operation =
    OrdinaryAffineProductTileWithTailTransform.transform(op, tileSize, requireReductionLoop = true)

final class OrdinaryAffineProductLoopTileWithTail(ctx: MLContext, tileSize: BigInt) extends ModulePass(ctx):
  override val name: String = "ordinary-affine-product-loop-tile-with-tail"

  override def transform(op: Operation): Operation =
    OrdinaryAffineProductTileWithTailTransform.transform(op, tileSize, requireReductionLoop = false)

private object OrdinaryAffineProductTileWithTailTransform:
  private def asIndex(v: Value[Attribute]): Operand[IndexType] =
    v.asInstanceOf[Operand[IndexType]]

  private def idxConst(v: BigInt): arith.Constant =
    arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

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

  private def collectLoopsInnermostFirst(op: Operation): Seq[affine.For] =
    val loops = mutable.ArrayBuffer.empty[affine.For]

    def visit(cur: Operation): Unit =
      cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
      cur match
        case loop: affine.For => loops += loop
        case _                => ()

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

  private def buildLoop(
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
      res = resultTypes.map(ty => Result(ty)),
      lowerBoundMap = lowerBoundMap,
      upperBoundMap = upperBoundMap,
      step = IntegerAttr(IntData(step), IndexType()),
      body = body,
    )

  private def hasTileMarker(loop: affine.For): Boolean =
    loop.attributes.contains("scair.ordinary_affine_product_tile_with_tail.mode") ||
      loop.attributes.contains("scair.ordinary_affine_product_tile_with_tail.generated")

  private def isStaticUnitStep(loop: affine.For): Boolean =
    loop.step.value.value == 1

  private def chooseProductUpperBound(loop: affine.For): Option[Value[Attribute]] =
    if loop.upperBoundOperands.size != 1 then None
    else
      loop.upperBoundOperands.head.owner match
        case Some(_: arith.MulI) => Some(loop.upperBoundOperands.head)
        case _                   => None

  private def tryTile(loop: affine.For, tileSize: BigInt, requireReductionLoop: Boolean): Boolean =
    if hasTileMarker(loop) then false
    else if loop.body.blocks.size != 1 then false
    else if requireReductionLoop && (loop.inits.isEmpty || loop.res.isEmpty) then false
    else if !isStaticUnitStep(loop) then false
    else if loop.lowerBoundOperands.size != 1 || loop.upperBoundOperands.size != 1 then false
    else if !isIdentityProjection(loop.lowerBoundMap) || !isIdentityProjection(loop.upperBoundMap) then false
    else
      chooseProductUpperBound(loop) match
        case None => false
        case Some(fullUpperBound) =>
          val oldBlock = loop.body.blocks.head
          val oldIv = oldBlock.arguments.head
          val oldIterArgs = oldBlock.arguments.tail
          val externalValues = collectExternalValues(oldBlock)
          val zero = idxConst(0)

          val outerLoop = buildLoop(
            zero.result,
            Seq(fullUpperBound),
            tileSize,
            loop.inits.map(_.asInstanceOf[Operand[Attribute]]),
            loop.res.map(_.typ),
            upperBoundMap = symbolIdentityMap,
          ) { outerArgs =>
            val tileIv = outerArgs.head
            val outerIterArgs = outerArgs.tail

            val innerLoop = buildLoop(
              tileIv,
              Seq(tileIv, fullUpperBound),
              1,
              outerIterArgs.map(_.asInstanceOf[Operand[Attribute]]),
              loop.res.map(_.typ),
              upperBoundMap = tileTailMap(tileSize),
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
            innerLoop.attributes.addOne(
              "scair.ordinary_affine_product_tile_with_tail.generated" -> StringData("inner")
            )

            Seq(innerLoop, affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])))
          }

          outerLoop.attributes.addOne("scair.ordinary_affine_product_tile_with_tail.mode" -> StringData("static_step_tail_guarded"))
          outerLoop.attributes.addOne("scair.ordinary_affine_product_tile_with_tail.tail_free" -> StringData("false"))
          outerLoop.attributes.addOne("scair.ordinary_affine_product_tile_with_tail.proof" -> StringData("none"))
          outerLoop.attributes.addOne("scair.ordinary_affine_product_tile_with_tail.generated" -> StringData("outer"))

          RewriteMethods.replaceOp(
            loop,
            Seq(zero, outerLoop),
            Some(outerLoop.results),
          )
          true

  def transform(op: Operation, tileSize: BigInt, requireReductionLoop: Boolean): Operation =
    require(tileSize > 0, s"ordinary affine tile size must be positive, got $tileSize")
    var changed = true
    while changed do
      changed = false
      collectLoopsInnermostFirst(op).foreach {
        case loop: affine.For =>
          if loop.containerBlock.nonEmpty && tryTile(loop, tileSize, requireReductionLoop) then
            changed = true
      }
    op
