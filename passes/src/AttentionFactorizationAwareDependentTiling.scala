package scair.passes.attention_factorization_aware_dependent_tiling

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.NatProvenance
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

import scala.collection.mutable

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def idxConst(v: BigInt): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def toIndex(nat: Value[Attribute]): dTensor.ShapeToIndex =
  dTensor.ShapeToIndex(
    nat.asInstanceOf[Operand[dTensor.dTensorNatType]],
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
    outerUpperBound: Value[Attribute],
    innerUpperBound: Value[Attribute],
)

private def choosePlan(loop: d_affine.For): Option[TilePlan] =
  NatProvenance.resolveNat(loop.upperBoundOperands.head).flatMap(_.owner) match
    case Some(dTensor.NatMul(lhs, rhs, _)) =>
      val outerIdx = toIndex(lhs)
      val innerIdx = toIndex(rhs)
      Some(
        TilePlan(
          prelude = Seq(outerIdx, innerIdx),
          outerUpperBound = outerIdx.res,
          innerUpperBound = innerIdx.res,
        )
      )
    case _ => None

private def buildLoop(
    lowerBound: Value[Attribute],
    upperBound: Value[Attribute],
    step: IntegerAttr,
    inits: Seq[Operand[Attribute]],
    resultTypes: Seq[Attribute],
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
    inits = inits,
    res = resultTypes.map(ty => Result(ty)),
    lowerBoundMap = identityMap,
    upperBoundMap = identityMap,
    step = step,
    body = body,
  )

private def hasTileMarker(loop: d_affine.For): Boolean =
  loop.attributes.contains("scair.attention.tile.mode") ||
  loop.attributes.contains("scair.attention.tile.generated")

private def tryTile(loop: d_affine.For): Boolean =
  if hasTileMarker(loop) then false
  else if loop.body.blocks.size != 1 then false
  else if loop.inits.isEmpty || loop.res.isEmpty then false
  else if loop.step.value.value != 1 then false
  else if loop.lowerBoundOperands.size != 1 || loop.upperBoundOperands.size != 1 then false
  else if !isIdentityProjection(loop.lowerBoundMap) || !isIdentityProjection(loop.upperBoundMap) then false
  else if NatProvenance.exactConst(loop.lowerBoundOperands.head) != Some(0) then false
  else
    choosePlan(loop) match
      case None => false
      case Some(plan) =>
        val oldBlock = loop.body.blocks.head
        val oldIv = oldBlock.arguments.head
        val oldIterArgs = oldBlock.arguments.tail
        val externalValues = collectExternalValues(oldBlock)
        val zero = idxConst(0)

        val outerLoop = buildLoop(
          zero.result,
          plan.outerUpperBound,
          IntegerAttr(IntData(1), I32),
          loop.inits.map(_.asInstanceOf[Operand[Attribute]]),
          loop.res.map(_.typ),
        ) { outerArgs =>
          val outerIv = outerArgs.head
          val outerIterArgs = outerArgs.tail
          val innerLoop = buildLoop(
            zero.result,
            plan.innerUpperBound,
            IntegerAttr(IntData(1), I32),
            outerIterArgs.map(_.asInstanceOf[Operand[Attribute]]),
            loop.res.map(_.typ),
          ) { innerArgs =>
            val innerIv = innerArgs.head
            val innerIterArgs = innerArgs.tail
            val mul = arith.MulI(
              outerIv.asInstanceOf[Operand[arith.AnyIntegerType]],
              plan.innerUpperBound.asInstanceOf[Operand[arith.AnyIntegerType]],
              Result(IndexType()),
            )
            val add = arith.AddI(
              mul.result.asInstanceOf[Operand[arith.AnyIntegerType]],
              innerIv.asInstanceOf[Operand[arith.AnyIntegerType]],
              Result(IndexType()),
            )
            given mutable.Map[Block, Block] = mutable.Map.empty
            val valueMapper = mutable.Map[Value[Attribute], Value[Attribute]](
              oldIv -> add.result
            )
            externalValues.foreach(v => valueMapper.update(v, v))
            oldIterArgs.zip(innerIterArgs).foreach { case (oldArg, newArg) =>
              valueMapper.update(oldArg, newArg)
            }
            given mutable.Map[Value[Attribute], Value[Attribute]] = valueMapper
            Seq(mul, add) ++ oldBlock.operations.map(_.deepCopy).toSeq
          }
          innerLoop.attributes.addOne(
            "scair.attention.tile.generated" -> StringData("inner")
          )

          Seq(
            innerLoop,
            d_affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])),
          )
        }

        outerLoop.attributes.addOne(
          "scair.attention.tile.mode" -> StringData("factorized_tail_free")
        )
        outerLoop.attributes.addOne(
          "scair.attention.tile.tail_free" -> StringData("true")
        )
        outerLoop.attributes.addOne(
          "scair.attention.tile.generated" -> StringData("outer")
        )

        RewriteMethods.replaceOp(
          loop,
          zero +: (plan.prelude :+ outerLoop),
          Some(outerLoop.results),
        )
        true

final class AttentionFactorizationAwareDependentTiling(ctx: MLContext)
    extends ModulePass(ctx):
  override val name: String = "attention-factorization-aware-dependent-tiling"

  override def transform(op: Operation): Operation =
    var changed = true
    while changed do
      changed = false
      collectLoopsInnermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryTile(loop) then changed = true
      }
    op
