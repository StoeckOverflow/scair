package scair.passes.licm_helpers

import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.ir.*

import scala.collection.mutable

trait LoopLikeAdapter:
  def loopOp: Operation
  def loopRegions: Seq[Region]
  def topLevelOps: Seq[Operation]
  def isDefinedOutsideLoop(
      v: Value[Attribute],
      hoistedOps: Set[Operation],
  ): Boolean
  def rebuildWithHoisted(opsToHoist: Seq[Operation]): (Seq[Operation], Operation)

object LoopInvariantCodeMotion:

  def isLICMMovable(op: Operation): Boolean =
    if op.isInstanceOf[IsTerminator] || op.regions.nonEmpty then false
    else
      op match
        case _: arith.Constant => true
        case _: arith.AddI     => true
        case _: arith.MulI     => true
        case _: arith.AddF     => true
        case _                 => false

  def findHoistableTopLevelOps(
      loop: LoopLikeAdapter,
      shouldMove: Operation => Boolean = isLICMMovable,
  ): Seq[Operation] =
    val hoistable = mutable.ArrayBuffer.empty[Operation]
    val hoistedOps = mutable.LinkedHashSet.empty[Operation]

    loop.topLevelOps.foreach { op =>
      if shouldMove(op) &&
          op.operands.forall(v => loop.isDefinedOutsideLoop(v, hoistedOps.toSet))
      then
        hoistable += op
        hoistedOps += op
    }

    hoistable.toSeq

final case class DAffineForLoopAdapter(loop: d_affine.For) extends LoopLikeAdapter:
  override def loopOp: Operation = loop
  override def loopRegions: Seq[Region] = Seq(loop.body)

  private def bodyBlock: Block = loop.body.blocks.head

  override def topLevelOps: Seq[Operation] =
    bodyBlock.operations.toSeq.filterNot(_.isInstanceOf[d_affine.Yield])

  override def isDefinedOutsideLoop(
      v: Value[Attribute],
      hoistedOps: Set[Operation],
  ): Boolean =
    v.owner match
      case Some(owner: Operation) =>
        hoistedOps.contains(owner) || !loop.isAncestor(owner)
      case Some(owner: Block) =>
        !loop.isAncestor(owner)
      case None =>
        true

  override def rebuildWithHoisted(
      opsToHoist: Seq[Operation]
  ): (Seq[Operation], Operation) =
    val hoistSet = opsToHoist.toSet
    given mutable.Map[Block, Block] = mutable.Map.empty
    val hoistedValueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]

    val hoistedCopies = opsToHoist.map { op =>
      val copied = op.deepCopy(using mutable.Map.empty[Block, Block], hoistedValueMap)
      hoistedValueMap.addAll(op.results.zip(copied.results))
      copied
    }

    val newBody = Block(bodyBlock.arguments.map(_.typ), Seq.empty)
    val blockMap = mutable.Map[Block, Block](bodyBlock -> newBody)
    val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
    valueMap.addAll(bodyBlock.arguments.zip(newBody.arguments))
    valueMap.addAll(hoistedValueMap)

    bodyBlock.operations.foreach { op =>
      if !hoistSet.contains(op) then
        val copied = op.deepCopy(using blockMap, valueMap)
        newBody.addOp(copied)
        valueMap.addAll(op.results.zip(copied.results))
    }

    val rebuiltLoop = d_affine.For(
      loop.lowerBoundOperands,
      loop.upperBoundOperands,
      loop.stepOperands,
      loop.inits,
      loop.res.map(r => Result(r.typ)),
      loop.lowerBoundMap,
      loop.upperBoundMap,
      loop.step,
      Region(Seq(newBody)),
    )

    (hoistedCopies, rebuiltLoop)
