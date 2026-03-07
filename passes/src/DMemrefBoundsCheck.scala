package scair.passes.d_memref_bounds

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.exceptions.VerifyException
import scair.ir.*
import scair.transformations.ModulePass
import scair.utils.OK

import scala.collection.mutable

final class DMemrefBoundsCheck(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "d-memref-bounds-check"

  private def normalizeNat(v: Value[Attribute]): Value[Attribute] =
    dTensorTypeUtil.resolveNatValue(v) match
      case OK(base) => base
      case _        => v

  private def sameNat(a: Value[Attribute], b: Value[Attribute]): Boolean =
    normalizeNat(a) eq normalizeNat(b)

  private def exactNat(
      v: Value[Attribute],
      memo: mutable.Map[Value[Attribute], Option[BigInt]],
      inProgress: mutable.Set[Value[Attribute]],
  ): Option[BigInt] =
    val n = normalizeNat(v)
    memo.getOrElseUpdate(
      n, {
        if inProgress.contains(n) then None
        else
          inProgress += n
          val out: Option[BigInt] = n.owner match
            case Some(NatConst(IntegerAttr(IntData(c), _), _)) =>
              Some(c)
            case Some(NatAdd(lhs, rhs, _)) =>
              for
                l <- exactNat(lhs, memo, inProgress)
                r <- exactNat(rhs, memo, inProgress)
              yield l + r
            case Some(NatMul(lhs, rhs, _)) =>
              for
                l <- exactNat(lhs, memo, inProgress)
                r <- exactNat(rhs, memo, inProgress)
              yield l * r
            case Some(d_affine.Min(lhs, rhs, _)) =>
              for
                l <- exactNat(lhs, memo, inProgress)
                r <- exactNat(rhs, memo, inProgress)
              yield l.min(r)
            case _ => None
          inProgress -= n
          out
      },
    )

  private def loopIvUpperBound(iv: Value[Attribute]): Option[Value[Attribute]] =
    iv.owner match
      case Some(b: Block) =>
        b.containerRegion.flatMap(_.containerOperation) match
          case Some(d_affine.For(_, ub, _, _)) if b.arguments.nonEmpty && (b.arguments.head eq iv) =>
            Some(ub)
          case _ => None
      case _ => None

  private def checkIndexLtDim(
      idx: Value[Attribute],
      dim: Value[Attribute],
      opName: String,
      axis: Int,
      memo: mutable.Map[Value[Attribute], Option[BigInt]],
      inProgress: mutable.Set[Value[Attribute]],
  ): Unit =
    // Safe by loop semantics: iv in d_affine.for always satisfies iv < ub in body.
    val safeByLoop =
      loopIvUpperBound(idx).exists(ub => sameNat(ub, dim))
    if safeByLoop then return

    val idxConst = exactNat(idx, memo, inProgress)
    val dimConst = exactNat(dim, memo, inProgress)
    (idxConst, dimConst) match
      case (Some(i), Some(d)) if i >= d =>
        throw VerifyException(
          s"d_memref-bounds: `$opName` index $axis provably out of bounds ($i >= $d)"
        )
      case _ => ()

  private def checkSubviewBound(
      off: Value[Attribute],
      size: Value[Attribute],
      dim: Value[Attribute],
      axis: Int,
      memo: mutable.Map[Value[Attribute], Option[BigInt]],
      inProgress: mutable.Set[Value[Attribute]],
  ): Unit =
    val offConst = exactNat(off, memo, inProgress)
    val sizeConst = exactNat(size, memo, inProgress)
    val dimConst = exactNat(dim, memo, inProgress)

    (offConst, sizeConst, dimConst) match
      case (Some(o), Some(s), Some(d)) if o + s > d =>
        throw VerifyException(
          s"d_memref-bounds: `d_memref.subview` axis $axis provably out of bounds ($o + $s > $d)"
        )
      case _ =>
        // Fast safe pattern used by shape-preserving subviews.
        val isZeroOffset = offConst.contains(0)
        if isZeroOffset && sameNat(size, dim) then ()
        else ()

  private def walk(
      op: Operation,
      memo: mutable.Map[Value[Attribute], Option[BigInt]],
      inProgress: mutable.Set[Value[Attribute]],
  ): Unit =
    op match
      case d_memref.Load(memref, indices, _) =>
        indices.zip(memref.typ.params).zipWithIndex.foreach { case ((idx, d), i) =>
          checkIndexLtDim(idx, d.getVal(), "d_memref.load", i, memo, inProgress)
        }
      case d_memref.Store(_, memref, indices) =>
        indices.zip(memref.typ.params).zipWithIndex.foreach { case ((idx, d), i) =>
          checkIndexLtDim(idx, d.getVal(), "d_memref.store", i, memo, inProgress)
        }
      case d_memref.Subview(src, offsets, sizes, _) =>
        offsets.zip(sizes).zip(src.typ.params).zipWithIndex.foreach {
          case (((off, size), dim), axis) =>
            checkSubviewBound(off, size, dim.getVal(), axis, memo, inProgress)
        }
      case _ => ()

    op.regions.foreach(_.blocks.foreach(_.operations.foreach(walk(_, memo, inProgress))))

  override def transform(op: Operation): Operation =
    val memo = mutable.Map.empty[Value[Attribute], Option[BigInt]]
    val inProgress = mutable.Set.empty[Value[Attribute]]
    walk(op, memo, inProgress)
    op
