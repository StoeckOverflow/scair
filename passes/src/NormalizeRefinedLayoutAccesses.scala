package scair.passes.normalize_refined_layout_accesses

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

// Layout parameters may be stored either as index-like integers or as dtensor
// nat values. This helper normalizes both cases to index SSA values so the
// subsequent address arithmetic is purely arithmetic IR.
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

private def isCanonicalFlatCarrier(ty: d_memref.dMemrefMemrefType): Boolean =
  ty.params.size == 1 &&
  ty.offset.isEmpty &&
  ty.strides.isEmpty

private def underlyingFlatBuffer(
    memref: Operand[d_memref.dMemrefMemrefType]
): Option[Operand[d_memref.dMemrefMemrefType]] =
  memref.owner match
    case Some(cast: d_memref.Cast) =>
      underlyingFlatBuffer(cast.src)
    case Some(rc: d_memref.ReinterpretCast) if isCanonicalFlatCarrier(rc.src.typ) =>
      Some(rc.src)
    case _ =>
      None

private def linearizedIndexOps(
    ty: d_memref.dMemrefMemrefType,
    indices: Seq[Operand[IndexType]],
): (Vector[Operation], Value[Attribute]) =
  val rank = indices.size
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
  val indexTerms = indices.zip(strideOpsAndValues).map { case (idx, (prefix, stride)) =>
    val mul = mulIndex(idx, stride)
    (prefix :+ mul, mul.result)
  }
  val prefix = Vector.newBuilder[Operation]
  prefix ++= offsetOpsAndValue._1
  indexTerms.foreach(prefix ++= _._1)
  val linear =
    if rank == 0 then offsetOpsAndValue._2.get
    else
      val withOffset = offsetOpsAndValue._2.toSeq ++ indexTerms.map(_._2)
      withOffset.reduceLeft { (lhs, rhs) =>
        val add = addIndex(lhs, rhs)
        prefix += add
        add.result
      }
  (prefix.result(), linear)

private def composeSubviewIndices(
    subview: d_memref.Subview,
    indices: Seq[Operand[IndexType]],
): (Vector[Operation], Seq[Operand[IndexType]]) =
  val prefix = Vector.newBuilder[Operation]
  val composed = indices.zip(subview.offsets.zip(subview.strides)).map { case (idx, (off, stride)) =>
    val scaled =
      stride.owner match
        case Some(arith.Constant(IntegerAttr(IntData(1), _: IndexType), _)) =>
          idx.asInstanceOf[Value[Attribute]]
        case _ =>
          val mul = mulIndex(idx.asInstanceOf[Value[Attribute]], stride.asInstanceOf[Value[Attribute]])
          prefix += mul
          mul.result
    val shifted =
      off.owner match
        case Some(arith.Constant(IntegerAttr(IntData(0), _: IndexType), _)) =>
          scaled
        case _ =>
          val add = addIndex(scaled, off.asInstanceOf[Value[Attribute]])
          prefix += add
          add.result
    asIndex(shifted)
  }
  (prefix.result(), composed)

private val NormalizeLoad = pattern {
  case op: d_memref.Load =>
    op.memref.owner match
      case Some(sv: d_memref.Subview) =>
        val (prefix, composed) = composeSubviewIndices(sv, op.indices)
        val normalized = d_memref.Load(sv.src, composed, Result(op.res.typ))
        (prefix :+ normalized, Seq(normalized.res))
      case _ =>
        PatternAction.Abort
}

private val NormalizeSubviewStore = pattern {
  case op: d_memref.Store =>
    op.memref.owner match
      case Some(sv: d_memref.Subview) =>
        val (prefix, composed) = composeSubviewIndices(sv, op.indices)
        prefix :+ d_memref.Store(op.value, sv.src, composed)
      case _ =>
        PatternAction.Abort
}

private val NormalizeStridedLoad = pattern {
  case op: d_memref.Load if op.memref.typ.strides.nonEmpty =>
    underlyingFlatBuffer(op.memref).map { flat =>
      val (prefix, linear) = linearizedIndexOps(op.memref.typ, op.indices)
      val normalized = d_memref.Load(flat, Seq(asIndex(linear)), Result(op.res.typ))
      (prefix :+ normalized, Seq(normalized.res))
    }.getOrElse(PatternAction.Abort)
}

private val NormalizeStridedStore = pattern {
  case op: d_memref.Store if op.memref.typ.strides.nonEmpty =>
    underlyingFlatBuffer(op.memref).map { flat =>
      val (prefix, linear) = linearizedIndexOps(op.memref.typ, op.indices)
      prefix :+ d_memref.Store(op.value, flat, Seq(asIndex(linear)))
    }.getOrElse(PatternAction.Abort)
}

// Rewrites refined view accesses into explicit linearized arithmetic plus
// ordinary flat-buffer d_memref.load/store operations.
final class NormalizeRefinedLayoutAccesses(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "normalize-refined-layout-accesses"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(NormalizeLoad, NormalizeSubviewStore, NormalizeStridedLoad, NormalizeStridedStore)
      )
    )
