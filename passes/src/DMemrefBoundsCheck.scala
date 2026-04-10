package scair.passes.d_memref_bounds

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.dialects.arith
import scair.exceptions.VerifyException
import scair.ir.*
import scair.passes.NatProvenance
import scair.transformations.ModulePass

/**
 * Verifies that `d_memref` accesses are provably in bounds.
 *
 * This pass walks `d_memref.load`, `d_memref.store`, and `d_memref.subview`
 * operations, using loop structure and nat provenance to prove simple index and
 * slice bounds facts. It is a checking pass: safe operations are kept unchanged,
 * and provably out-of-bounds operations cause verification to fail.
 */
final class DMemrefBoundsCheck(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "d-memref-bounds-check"

  private def recoverProjectedBoundOperand(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    if map.affineMap.affineExprs.size != 1 then None
    else
      val dimNames = map.affineMap.dimensions
      val symNames = map.affineMap.symbols
      val dimCount = dimNames.size
      if operands.size != dimCount + symNames.size then None
      else
        map.affineMap.affineExprs.head match
          case AffineDimExpr(position) =>
            val idx = dimNames.indexOf(position)
            if idx < 0 then None else Some(operands(idx))
          case AffineSymExpr(position) =>
            val idx = symNames.indexOf(position)
            if idx < 0 then None else Some(operands(dimCount + idx))
          case _ => None

  private def loopIvUpperBound(iv: Value[Attribute]): Option[Value[Attribute]] =
    iv.owner match
      case Some(b: Block) =>
        b.containerRegion.flatMap(_.containerOperation) match
          case Some(
                d_affine.For(_, ubOperands, _, _, _, ubMap, _, _)
              ) if b.arguments.nonEmpty && (b.arguments.head eq iv) =>
            recoverProjectedBoundOperand(ubOperands, ubMap)
          case _ => None
      case _ => None

  private def checkIndexLtDim(
      idx: Value[Attribute],
      dim: Value[Attribute],
      opName: String,
      axis: Int,
  ): Unit =
    val safeByLoop =
      loopIvUpperBound(idx).exists(ub => NatProvenance.sameNat(ub, dim))
    if safeByLoop then return

    val idxConst = NatProvenance.exactConst(idx)
    val dimConst = NatProvenance.exactConst(dim)
    (idxConst, dimConst) match
      case (Some(i), _) if i < 0 =>
        throw VerifyException(
          s"d_memref-bounds: `$opName` index $axis provably out of bounds ($i < 0)"
        )
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
  ): Unit =
    val offConst = NatProvenance.exactConst(off)
    val sizeConst = NatProvenance.exactConst(size)
    val dimConst = NatProvenance.exactConst(dim)

    (offConst, sizeConst, dimConst) match
      case (Some(o), _, _) if o < 0 =>
        throw VerifyException(
          s"d_memref-bounds: `d_memref.subview` axis $axis provably out of bounds (offset $o < 0)"
        )
      case (_, Some(s), _) if s < 0 =>
        throw VerifyException(
          s"d_memref-bounds: `d_memref.subview` axis $axis provably out of bounds (size $s < 0)"
        )
      case (Some(o), Some(s), Some(d)) if o + s > d =>
        throw VerifyException(
          s"d_memref-bounds: `d_memref.subview` axis $axis provably out of bounds ($o + $s > $d)"
        )
      case _ =>
        val isZeroOffset = offConst.contains(0)
        if isZeroOffset && NatProvenance.sameNat(size, dim) then ()
        else ()

  private def dimValue(dim: d_memref.DimParam): Value[Attribute] =
    dim match
      case d: ValueAttribute =>
        d.getVal()
      case IntegerAttr(IntData(v), _: IndexType | _: IntegerType) =>
        arith.Constant(
          IntegerAttr(IntData(v), IndexType()),
          Result(IndexType()),
        ).result

  private def walk(op: Operation): Unit =
    op match
      case d_memref.Load(memref, indices, _) =>
        indices.zip(memref.typ.params).zipWithIndex.foreach { case ((idx, d), i) =>
          checkIndexLtDim(idx, dimValue(d), "d_memref.load", i)
        }
      case d_memref.Store(_, memref, indices) =>
        indices.zip(memref.typ.params).zipWithIndex.foreach { case ((idx, d), i) =>
          checkIndexLtDim(idx, dimValue(d), "d_memref.store", i)
        }
      case d_memref.Subview(src, offsets, sizes, _, _) =>
        offsets.zip(sizes).zip(src.typ.params).zipWithIndex.foreach {
          case (((off, size), dim), axis) =>
            checkSubviewBound(off, size, dimValue(dim), axis)
        }
      case _ => ()

    op.regions.foreach(_.blocks.foreach(_.operations.foreach(walk)))

  override def transform(op: Operation): Operation =
    walk(op)
    op
