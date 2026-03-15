package scair.passes.hoist_refined_layout_invariants

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private val HoistRowBase = pattern {
  case outer: d_affine.For if outer.body.blocks.size == 1 =>
    val outerBody = outer.body.blocks.head
    outerBody.operations.toSeq match
      case Seq(inner: d_affine.For, y: d_affine.Yield) if inner.body.blocks.size == 1 =>
        val innerBody = inner.body.blocks.head
        val newOuterBody = Block(outerBody.arguments.map(_.typ), Seq.empty)
        val outerValueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
        outerValueMap.addAll(outerBody.arguments.zip(newOuterBody.arguments))
        val outerIv = outerBody.arguments.head
        val newOuterIv = newOuterBody.arguments.head
        val hoistedOps = mutable.ArrayBuffer.empty[Operation]
        val rebuiltInnerBody = Block(innerBody.arguments.map(_.typ), Seq.empty)
        val blockMap = mutable.Map[Block, Block](innerBody -> rebuiltInnerBody)
        val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
        valueMap.addAll(innerBody.arguments.zip(rebuiltInnerBody.arguments))

        innerBody.operations.foreach {
          case mul: arith.MulI if mul.lhs == outerIv.asInstanceOf[Operand[IndexType]] || mul.rhs == outerIv.asInstanceOf[Operand[IndexType]] =>
            val hoisted = arith.MulI(
              (if mul.lhs == outerIv.asInstanceOf[Operand[IndexType]] then newOuterIv else outerValueMap.getOrElse(mul.lhs, mul.lhs)).asInstanceOf[Operand[IndexType]],
              (if mul.rhs == outerIv.asInstanceOf[Operand[IndexType]] then newOuterIv else outerValueMap.getOrElse(mul.rhs, mul.rhs)).asInstanceOf[Operand[IndexType]],
              Result(mul.result.typ),
            )
            hoistedOps += hoisted
            valueMap(mul.result) = hoisted.result
          case base: d_memref.BasePtr =>
            val hoisted = d_memref.BasePtr(
              outerValueMap.getOrElse(base.memref, base.memref).asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
              Result(base.res.typ),
            )
            hoistedOps += hoisted
            valueMap(base.res) = hoisted.res
          case other =>
            val copied = other.deepCopy(using blockMap, valueMap)
            rebuiltInnerBody.addOp(copied)
            valueMap.addAll(other.results.zip(copied.results))
        }

        if hoistedOps.isEmpty then
          PatternAction.Abort
        else
          val outerRemap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
          outerRemap.addAll(outerBody.arguments.zip(newOuterBody.arguments))
          val rebuiltInner = d_affine.For(
            inner.lowerBoundOperands.map(v => outerValueMap.getOrElse(v, v).asInstanceOf[Operand[IndexType]]),
            inner.upperBoundOperands.map(v => outerValueMap.getOrElse(v, v).asInstanceOf[Operand[IndexType]]),
            inner.inits.map(v => outerValueMap.getOrElse(v, v).asInstanceOf[Operand[Attribute]]),
            inner.res.map(r => Result(r.typ)),
            inner.lowerBoundMap,
            inner.upperBoundMap,
            inner.step,
            Region(Seq(rebuiltInnerBody)),
          )
          outerRemap.addAll(inner.results.zip(rebuiltInner.results))
          val copiedYield = y.deepCopy(using mutable.Map.empty[Block, Block], outerRemap)
          hoistedOps.foreach(newOuterBody.addOp)
          newOuterBody.addOp(rebuiltInner)
          newOuterBody.addOp(copiedYield)
          val rebuiltOuter = d_affine.For(
            outer.lowerBoundOperands,
            outer.upperBoundOperands,
            outer.inits,
            outer.res.map(r => Result(r.typ)),
            outer.lowerBoundMap,
            outer.upperBoundMap,
            outer.step,
            Region(Seq(newOuterBody)),
          )
          rebuiltOuter
      case _ =>
        PatternAction.Abort
}

final class HoistRefinedLayoutInvariants(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "hoist-refined-layout-invariants"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(HoistRowBase)))
