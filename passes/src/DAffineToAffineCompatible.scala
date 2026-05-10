package scair.passes.d_affine_to_affine_compatible

import scair.MLContext
import scair.dialects.affine
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.ir.*
import scair.transformations.ModulePass
import scair.transformations.RewriteMethods

import scala.collection.mutable

private enum YieldDialect:
  case Affine
  case DAffine

private def expectedArity(map: AffineMapAttr): Int =
  map.affineMap.dimensions.size + map.affineMap.symbols.size

private def isEligible(loop: d_affine.For): Boolean =
  loop.stepOperands.isEmpty &&
    loop.step.value.value > 0 &&
    loop.body.blocks.size == 1 &&
    loop.inits.size == loop.res.size &&
    loop.lowerBoundMap.affineMap.affineExprs.size == 1 &&
    loop.upperBoundMap.affineMap.affineExprs.size == 1 &&
    loop.lowerBoundOperands.size == expectedArity(loop.lowerBoundMap) &&
    loop.upperBoundOperands.size == expectedArity(loop.upperBoundMap) &&
    loop.body.blocks.head.arguments.size == 1 + loop.inits.size &&
    loop.body.blocks.head.arguments.head.typ == IndexType() &&
    loop.body.blocks.head.arguments.tail.zip(loop.inits).forall { case (arg, init) =>
      arg.typ == init.typ
    } &&
    loop.body.blocks.head.operations.lastOption.exists {
      case yieldOp: d_affine.Yield =>
        yieldOp.args.size == loop.res.size &&
          yieldOp.args.zip(loop.res).forall { case (arg, res) => arg.typ == res.typ }
      case _ => false
    }

private def collectOutermostEligibleLoops(op: Operation): Seq[d_affine.For] =
  val loops = mutable.ArrayBuffer.empty[d_affine.For]

  def visit(cur: Operation): Unit =
    cur match
      case loop: d_affine.For if isEligible(loop) =>
        loops += loop
      case _ =>
        cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

  visit(op)
  loops.toSeq

private final class DAffineToAffineConverter:
  private val blockMapper = mutable.Map.empty[Block, Block]
  private val valueMapper =
    mutable.Map.empty[Value[Attribute], Value[Attribute]]

  private def mapped[T <: Attribute](value: Value[T]): Value[T] =
    valueMapper
      .getOrElse(value.asInstanceOf[Value[Attribute]], value.asInstanceOf[Value[Attribute]])
      .asInstanceOf[Value[T]]

  private def index(value: Value[IndexType]): Operand[IndexType] =
    mapped(value)

  private def operand(value: Value[Attribute]): Operand[Attribute] =
    mapped(value)

  def convertLoop(loop: d_affine.For): affine.For =
    val copiedResults = loop.res.map(result => Result(result.typ))
    loop.res.zip(copiedResults).foreach { case (oldResult, newResult) =>
      valueMapper.update(oldResult, newResult)
    }

    val converted = affine.For(
      lowerBoundOperands = loop.lowerBoundOperands.map(index),
      upperBoundOperands = loop.upperBoundOperands.map(index),
      inits = loop.inits.map(operand),
      res = copiedResults,
      lowerBoundMap = loop.lowerBoundMap,
      upperBoundMap = loop.upperBoundMap,
      step = loop.step,
      body = cloneRegion(loop.body, YieldDialect.Affine),
    )
    converted.attributes.addAll(loop.attributes)
    converted

  private def cloneRegion(region: Region, yieldDialect: YieldDialect): Region =
    Region(region.blocks.map(block => cloneBlock(block, yieldDialect)))

  private def cloneBlock(block: Block, yieldDialect: YieldDialect): Block =
    Block(
      block.arguments.map(_.typ),
      args =>
        block.arguments.zip(args).foreach { case (oldArg, newArg) =>
          valueMapper.update(oldArg, newArg)
        }
        block.operations.map(op => cloneOp(op, yieldDialect)).toSeq,
    )

  private def cloneDAffineLoop(loop: d_affine.For): d_affine.For =
    val copiedResults = loop.res.map(result => Result(result.typ))
    loop.res.zip(copiedResults).foreach { case (oldResult, newResult) =>
      valueMapper.update(oldResult, newResult)
    }

    val copied = d_affine.For(
      lowerBoundOperands = loop.lowerBoundOperands.map(index),
      upperBoundOperands = loop.upperBoundOperands.map(index),
      stepOperands = loop.stepOperands.map(index),
      inits = loop.inits.map(operand),
      res = copiedResults,
      lowerBoundMap = loop.lowerBoundMap,
      upperBoundMap = loop.upperBoundMap,
      step = loop.step,
      body = cloneRegion(loop.body, YieldDialect.DAffine),
    )
    copied.attributes.addAll(loop.attributes)
    copied

  private def cloneOp(op: Operation, yieldDialect: YieldDialect): Operation =
    op match
      case loop: d_affine.For if isEligible(loop) =>
        convertLoop(loop)
      case loop: d_affine.For =>
        cloneDAffineLoop(loop)
      case app: d_affine.Apply =>
        val copied = affine.Apply(
          mapOperands = (app.dimOperands ++ app.symbolOperands).map(index),
          res = Result(IndexType()),
          map = app.map,
        )
        valueMapper.update(app.res, copied.res)
        copied
      case min: d_affine.Min =>
        val copied = affine.Min(
          arguments = (min.dimOperands ++ min.symbolOperands).map(index),
          result = Result(IndexType()),
          map = min.map,
        )
        valueMapper.update(min.res, copied.result)
        copied
      case yieldOp: d_affine.Yield =>
        yieldDialect match
          case YieldDialect.Affine =>
            affine.Yield(yieldOp.args.map(operand))
          case YieldDialect.DAffine =>
            d_affine.Yield(yieldOp.args.map(operand))
      case other =>
        given mutable.Map[Block, Block] = blockMapper
        given mutable.Map[Value[Attribute], Value[Attribute]] = valueMapper
        other.deepCopy

final class DAffineToAffineCompatible(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "d-affine-to-affine-compatible"

  override def transform(op: Operation): Operation =
    collectOutermostEligibleLoops(op).foreach { loop =>
      if loop.containerBlock.nonEmpty then
        val converted = DAffineToAffineConverter().convertLoop(loop)
        RewriteMethods.replaceOp(loop, Seq(converted), Some(converted.results))
    }
    op
