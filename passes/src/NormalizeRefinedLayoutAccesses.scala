package scair.passes.normalize_refined_layout_accesses

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_memref
import scair.dialects.llvm
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def materializeLayoutParam(param: d_memref.LayoutParam): (Vector[Operation], Value[Attribute]) =
  param match
    case i: IntegerAttr =>
      val c = arith.Constant(IntegerAttr(i.value, IndexType()), Result(IndexType()))
      (Vector(c), c.result)
    case v: ValueAttribute =>
      v.getVal().typ match
        case _: IndexType => (Vector.empty, v.getVal())
        case _: dTensor.dTensorNatType =>
          val cast = dTensor.ShapeToIndex(v.getVal().asInstanceOf[Operand[dTensor.dTensorNatType]], Result(IndexType()))
          (Vector(cast), cast.res)
        case ValueRefType(ref) =>
          materializeLayoutParam(ValueAttribute(ref.getVal()))

private def addIndex(lhs: Value[Attribute], rhs: Value[Attribute]): arith.AddI =
  arith.AddI(asIndex(lhs), asIndex(rhs), Result(IndexType()))

private def mulIndex(lhs: Value[Attribute], rhs: Value[Attribute]): arith.MulI =
  arith.MulI(asIndex(lhs), asIndex(rhs), Result(IndexType()))

private val NormalizeLoad = pattern {
  case op: d_memref.Load if op.memref.typ.strides.nonEmpty =>
    val ty = op.memref.typ
    val rank = op.indices.size
    val offsetOpsAndValue =
      ty.offset match
        case Some(IntegerAttr(IntData(v), _)) if v == 0 => (Vector.empty[Operation], None)
        case Some(off) =>
          val (ops, v) = materializeLayoutParam(off)
          (ops, Some(v))
        case None =>
          val zero = arith.Constant(idxAttr(0), Result(IndexType()))
          (Vector(zero), Some(zero.result))
    val strideOpsAndValues = ty.strides.get.map(materializeLayoutParam)
    val indexTerms = op.indices.zip(strideOpsAndValues).map { case (idx, (prefix, stride)) =>
      val mul = mulIndex(idx, stride)
      (prefix :+ mul, mul.result)
    }
    val prefix = Vector.newBuilder[Operation]
    prefix ++= offsetOpsAndValue._1
    indexTerms.foreach(prefix ++= _._1)
    val linear: Value[Attribute] =
      if rank == 0 then offsetOpsAndValue._2.get
      else
        val terms = indexTerms.map(_._2)
        val withOffset = offsetOpsAndValue._2.toSeq ++ terms
        withOffset.reduceLeft { (lhs, rhs) =>
          val add = addIndex(lhs, rhs)
          prefix += add
          add.result
        }
    val base = d_memref.BasePtr(op.memref, Result(llvm.Ptr()))
    val normalized = d_memref.LinearizedLoadFromBase(base.res, asIndex(linear), Result(op.res.typ))
    (prefix.result() ++ Seq(base, normalized), Seq(normalized.res))
}

final class NormalizeRefinedLayoutAccesses(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "normalize-refined-layout-accesses"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(NormalizeLoad)))
