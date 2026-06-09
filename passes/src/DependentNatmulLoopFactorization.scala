package scair.passes.dependent_natmul_loop_factorization

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.{d_tensor as DTensor}
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.NatProvenance
import scair.passes.analysis.NatProductFacts
import scair.passes.analysis.NatProductFacts.FactorSelectionPolicy
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

import scala.collection.mutable

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

private final case class FactorizationPlan(
    prelude: Seq[Operation],
    outerUpperBound: Value[Attribute],
    innerUpperBound: Value[Attribute],
)

// Structural product-loop factorization infrastructure. This pass deliberately
// rewrites flat product loops into nested loops; the generic tiling emitter lives
// in ValueDependentTiling.
private def choosePlan(
    loop: d_affine.For,
    factorPolicy: FactorSelectionPolicy,
): Option[FactorizationPlan] =
  if loop.upperBoundOperands.size != 1 then None
  else
    for
      product <- NatProductFacts.flattenProduct(loop.upperBoundOperands.head)
      innerFactor <- product.selectFactor(factorPolicy)
      residualFactors <- NatProductFacts.residualAfterRemovingFactorProduct(
        loop.upperBoundOperands.head,
        innerFactor.value,
      )
      if residualFactors.nonEmpty
      residual <- NatProductFacts.buildExplicitProduct(residualFactors)
    yield
      val (residualPrelude, residualNat) = residual
      val outerIdx = toIndex(residualNat)
      val innerIdx = toIndex(innerFactor.value)
      FactorizationPlan(
        prelude = residualPrelude ++ Seq(outerIdx, innerIdx),
        outerUpperBound = outerIdx.res,
        innerUpperBound = innerIdx.res,
      )

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
    stepOperands = Seq.empty,
    inits = inits,
    res = resultTypes.map(ty => Result(ty)),
    lowerBoundMap = identityMap,
    upperBoundMap = identityMap,
    step = step,
    body = body,
  )

private def tryFactorize(
    loop: d_affine.For,
    factorPolicy: FactorSelectionPolicy,
): Boolean =
  if loop.body.blocks.size != 1 then false
  else if loop.lowerBoundOperands.size != 1 || loop.upperBoundOperands.size != 1 then false
  else if !isIdentityProjection(loop.lowerBoundMap) || !isIdentityProjection(loop.upperBoundMap) then false
  else if NatProvenance.exactConst(loop.lowerBoundOperands.head) != Some(0) then false
  else
    choosePlan(loop, factorPolicy) match
      case None => false
      case Some(plan) =>
        if loop.stepOperands.isEmpty && loop.step.value.value == 1 then
          tryFactorizeUnitProductLoop(loop, plan)
        else if stepMatchesInnerFactor(loop, plan) then
          tryFactorizeTiledProductLoop(loop, plan)
        else false

private def stepMatchesInnerFactor(loop: d_affine.For, plan: FactorizationPlan): Boolean =
  loop.stepOperands match
    case Seq(dynamicStep) =>
      NatProvenance.equivalentNatOrConst(dynamicStep, plan.innerUpperBound)
    case Seq() =>
      NatProvenance.exactConst(plan.innerUpperBound).contains(loop.step.value.value)
    case _ => false

private def tryFactorizeUnitProductLoop(loop: d_affine.For, plan: FactorizationPlan): Boolean =
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
    Seq(
      innerLoop,
      d_affine.Yield(innerLoop.results.map(_.asInstanceOf[Operand[Attribute]])),
    )
  }

  RewriteMethods.replaceOp(
    loop,
    zero +: (plan.prelude :+ outerLoop),
    Some(outerLoop.results),
  )
  true

private def tryFactorizeTiledProductLoop(loop: d_affine.For, plan: FactorizationPlan): Boolean =
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
    val tileStart = arith.MulI(
      outerIv.asInstanceOf[Operand[arith.AnyIntegerType]],
      plan.innerUpperBound.asInstanceOf[Operand[arith.AnyIntegerType]],
      Result(IndexType()),
    )
    given mutable.Map[Block, Block] = mutable.Map.empty
    val valueMapper = mutable.Map[Value[Attribute], Value[Attribute]](
      oldIv -> tileStart.result
    )
    externalValues.foreach(v => valueMapper.update(v, v))
    oldIterArgs.zip(outerIterArgs).foreach { case (oldArg, newArg) =>
      valueMapper.update(oldArg, newArg)
    }
    given mutable.Map[Value[Attribute], Value[Attribute]] = valueMapper
    Seq(tileStart) ++ oldBlock.operations.map(_.deepCopy).toSeq
  }

  RewriteMethods.replaceOp(
    loop,
    zero +: (plan.prelude :+ outerLoop),
    Some(outerLoop.results),
  )
  true

final class DependentNatmulLoopFactorization(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-natmul-loop-factorization"

  override def transform(op: Operation): Operation =
    var changed = true
    while changed do
      changed = false
      collectLoopsInnermostFirst(op).foreach { loop =>
        if loop.containerBlock.nonEmpty && tryFactorize(loop, factorPolicy) then changed = true
      }
    op
