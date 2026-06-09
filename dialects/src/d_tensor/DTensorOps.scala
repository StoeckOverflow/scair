package scair.dialects.d_tensor

import scair.print.Printer
import scair.clair.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

final case class AssumeExtent(
    extent: Operand[IndexType]
) extends DerivedOperation["d_tensor.assume_extent"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if extent.typ == IndexType() then OK(this)
    else Err(s"d_tensor.assume_extent: expected index operand, got ${extent.typ}")

final case class Empty(
    res: Result[DTensorTensorType]
) extends DerivedOperation["d_tensor.empty"]
    with NoMemoryEffect derives OpDefs

final case class Fill(
    v: Operand[TypeAttribute],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.fill"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if v.typ == res.typ.elem then OK(this)
    else
      Err(
        s"d_tensor.fill: expected fill value type ${res.typ.elem}, got ${v.typ}"
      )

final case class Dim(
    t: Operand[DTensorTensorType],
    axis: IntegerAttr,
    res: Result[ValueRefType],
) extends DerivedOperation["d_tensor.dim"]
    with NoMemoryEffect derives OpDefs:

  def selectedDimValue: OK[Value[Attribute]] =
    val idx = axis.value.value
    val rank = BigInt(t.typ.params.size)
    if idx < 0 || idx >= rank then
      Err(s"d_tensor.dim: axis $idx out of bounds for rank ${t.typ.params.size}")
    else DTensorTypeUtil.valueDim(t.typ.params(idx.toInt), "d_tensor.dim")

  override def customVerify(): OK[Operation] =
    val axisTyOk = axis.typ == I32
    if !axisTyOk then
      Err(s"d_tensor.dim: expected i32 axis attribute, got ${axis.typ}")
    else
      selectedDimValue.flatMap(sel =>
        if res.typ.ref.getVal() eq sel then
          DTensorTypeUtil.resolveIndexValue(res.typ.ref.getVal()).map(_ => this)
        else
          Err(
            "d_tensor.dim: expected result !value<...> to reference the selected embedded dim"
          )
      )

final case class Add(
    lhs: Operand[DTensorTensorType],
    rhs: Operand[DTensorTensorType],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.add"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    DTensorTypeUtil
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "d_tensor.add")
      .map(_ => this)

final case class Mul(
    lhs: Operand[DTensorTensorType],
    rhs: Operand[DTensorTensorType],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.mul"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    DTensorTypeUtil
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "d_tensor.mul")
      .map(_ => this)

final case class Matmul(
    lhs: Operand[DTensorTensorType],
    rhs: Operand[DTensorTensorType],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.matmul"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    DTensorTypeUtil.checkMatmul(lhs.typ, rhs.typ, res.typ).map(_ => this)

final case class Cast(
    src: Operand[DTensorTensorType],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.cast"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_tensor.cast: expected equal element types, got ${src.typ
            .elem} and ${res.typ.elem}"
      )
    else if src.typ.params.size != res.typ.params.size then
      Err(
        s"d_tensor.cast: expected equal ranks, got ${src.typ.params
            .size} and ${res.typ.params.size}"
      )
    else if !DTensorTypeUtil.sameDims(src.typ.params, res.typ.params)
    then Err("d_tensor.cast: expected pairwise SSA-identical dims")
    else OK(this)

final case class ExpandShape(
    src: Operand[DTensorTensorType],
    outputShape: Seq[Operand[IndexType]],
    reassociation: ArrayAttribute[Attribute],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.expand_shape"]
    with NoMemoryEffect derives OpDefs:

  private def parseReassociationGroups: OK[Seq[Seq[Int]]] =
    reassociation.attrValues.zipWithIndex
      .foldLeft[OK[Seq[Seq[Int]]]](OK(Seq.empty)) {
        case (acc, (groupAttr, groupIdx)) =>
          acc.flatMap(groups =>
            groupAttr match
              case group: ArrayAttribute[?] =>
                group.attrValues.zipWithIndex
                  .foldLeft[OK[Seq[Int]]](OK(Seq.empty)) {
                    case (groupAcc, (idxAttr, idxPos)) =>
                      groupAcc.flatMap(indices =>
                        idxAttr match
                          case IntegerAttr(IntData(idx), I32) =>
                            OK(indices :+ idx.toInt)
                          case other =>
                            Err(
                              s"d_tensor.expand_shape: reassociation group $groupIdx index $idxPos must be an i32 integer attribute, got ${DTensorTypeUtil.renderAttr(other)}"
                            )
                      )
                  }
                  .map(groupIndices => groups :+ groupIndices)
              case other =>
                Err(
                  s"d_tensor.expand_shape: reassociation group $groupIdx must be an array attribute, got ${DTensorTypeUtil.renderAttr(other)}"
                )
          )
      }

  private def checkReassociation(
      groups: Seq[Seq[Int]],
      srcRank: Int,
      resRank: Int,
  ): OK[Unit] =
    if groups.size != srcRank then
      Err(
        s"d_tensor.expand_shape: expected $srcRank reassociation groups, got ${groups.size}"
      )
    else if groups.exists(_.isEmpty) then
      Err("d_tensor.expand_shape: reassociation groups must be non-empty")
    else if groups.exists(_.exists(_ < 0)) then
      Err("d_tensor.expand_shape: reassociation indices must be non-negative")
    else
      val flattened = groups.flatten
      val expected = 0 until resRank
      if flattened != expected then
        Err(
          s"d_tensor.expand_shape: reassociation must cover result dims contiguously as ${expected.mkString("[", ", ", "]")}"
        )
      else OK(())

  private def outputShapeMatchesResult(): OK[Unit] =
    if outputShape.size != res.typ.params.size then
      Err(
        s"d_tensor.expand_shape: expected ${res.typ.params.size} output shape operands, got ${outputShape.size}"
      )
    else
      outputShape.zip(res.typ.params).zipWithIndex
        .foldLeft[OK[Unit]](OK(())) { case (acc, ((operand, dim), idx)) =>
          acc.flatMap(_ =>
            DTensorTypeUtil.valueDim(dim, "d_tensor.expand_shape").flatMap(dimValue =>
              (DTensorTypeUtil.resolveIndexValue(operand), DTensorTypeUtil.resolveIndexValue(dimValue)) match
                case (OK(lhs), OK(rhs)) if lhs eq rhs => OK(())
                case _ =>
                  Err(
                    s"d_tensor.expand_shape: output shape operand $idx must be SSA-identical to result dimension $idx"
                  )
            )
          )
        }

  /** Verifies the local shape contract for collapse: reassociation groups must
    * cover source dimensions contiguously. Product-shaped result dimensions are
    * handled by canonicalization.
    */
  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_tensor.expand_shape: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if resRank < srcRank then
      Err(
        s"d_tensor.expand_shape: expected result rank >= source rank, got $srcRank -> $resRank"
      )
    else
      outputShapeMatchesResult().flatMap(_ =>
        parseReassociationGroups.flatMap(groups =>
          checkReassociation(groups, srcRank, resRank).map(_ => this)
        )
      )

final case class CollapseShape(
    src: Operand[DTensorTensorType],
    reassociation: ArrayAttribute[Attribute],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.collapse_shape"]
    with NoMemoryEffect derives OpDefs:

  private def parseReassociationGroups: OK[Seq[Seq[Int]]] =
    reassociation.attrValues.zipWithIndex
      .foldLeft[OK[Seq[Seq[Int]]]](OK(Seq.empty)) {
        case (acc, (groupAttr, groupIdx)) =>
          acc.flatMap(groups =>
            groupAttr match
              case group: ArrayAttribute[?] =>
                group.attrValues.zipWithIndex
                  .foldLeft[OK[Seq[Int]]](OK(Seq.empty)) {
                    case (groupAcc, (idxAttr, idxPos)) =>
                      groupAcc.flatMap(indices =>
                        idxAttr match
                          case IntegerAttr(IntData(idx), I32) =>
                            OK(indices :+ idx.toInt)
                          case other =>
                            Err(
                              s"d_tensor.collapse_shape: reassociation group $groupIdx index $idxPos must be an i32 integer attribute, got ${DTensorTypeUtil.renderAttr(other)}"
                            )
                      )
                  }
                  .map(groupIndices => groups :+ groupIndices)
              case other =>
                Err(
                  s"d_tensor.collapse_shape: reassociation group $groupIdx must be an array attribute, got ${DTensorTypeUtil.renderAttr(other)}"
                )
          )
      }

  private def checkReassociation(
      groups: Seq[Seq[Int]],
      srcRank: Int,
      resRank: Int,
  ): OK[Unit] =
    if groups.size != resRank then
      Err(
        s"d_tensor.collapse_shape: expected $resRank reassociation groups, got ${groups.size}"
      )
    else if groups.exists(_.isEmpty) then
      Err("d_tensor.collapse_shape: reassociation groups must be non-empty")
    else if groups.exists(_.exists(_ < 0)) then
      Err("d_tensor.collapse_shape: reassociation indices must be non-negative")
    else
      val flattened = groups.flatten
      val expected = 0 until srcRank
      if flattened != expected then
        Err(
          s"d_tensor.collapse_shape: reassociation must cover source dims contiguously as ${expected.mkString("[", ", ", "]")}"
        )
      else OK(())

  /** Baseline-style shape collapse: reassociation only records which source
    * dimensions become each result dimension. Product-shaped result dimensions
    * are materialized by `tensor-shape-canonicalize`, not asserted here.
    */
  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_tensor.collapse_shape: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if resRank > srcRank then
      Err(
        s"d_tensor.collapse_shape: expected result rank <= source rank, got $srcRank -> $resRank"
      )
    else
      parseReassociationGroups.flatMap(groups =>
        checkReassociation(groups, srcRank, resRank).map(_ => this)
      )

final case class SplitDim(
    src: Operand[DTensorTensorType],
    outer: Operand[IndexType],
    inner: Operand[IndexType],
    dim: IntegerAttr,
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.split_dim"]
    with NoMemoryEffect derives OpDefs:

  private def sameDim(lhs: DimParam, rhs: DimParam): Boolean =
    DTensorTypeUtil.sameDims(Seq(lhs), Seq(rhs))

  private def sameIndexValue(lhs: Value[Attribute], rhs: DimParam): Boolean =
    DTensorTypeUtil.valueDim(rhs, "d_tensor.split_dim") match
      case OK(rhsValue) =>
        (DTensorTypeUtil.resolveIndexValue(lhs), DTensorTypeUtil.resolveIndexValue(rhsValue)) match
          case (OK(lv), OK(rv)) => lv eq rv
          case _                => false
      case _ => false

  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_tensor.split_dim: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if resRank != srcRank + 1 then
      Err(
        s"d_tensor.split_dim: expected result rank = input rank + 1, got $srcRank -> $resRank"
      )
    else if dim.typ != I32 then
      Err(s"d_tensor.split_dim: expected i32 dim attribute, got ${dim.typ}")
    else
      val axis = dim.value.value
      if axis < 0 || axis >= srcRank then
        Err(s"d_tensor.split_dim: dim $axis out of bounds for rank $srcRank")
      else
        val idx = axis.toInt
        val prefixOk = (0 until idx).forall(i => sameDim(src.typ.params(i), res.typ.params(i)))
        val suffixOk = ((idx + 1) until srcRank).forall(i =>
          sameDim(src.typ.params(i), res.typ.params(i + 1))
        )
        if !prefixOk then
          Err("d_tensor.split_dim: expected dimensions before split dim to be SSA-identical")
        else if !suffixOk then
          Err("d_tensor.split_dim: expected dimensions after split dim to be shifted and SSA-identical")
        else if !sameIndexValue(outer, res.typ.params(idx)) then
          Err(s"d_tensor.split_dim: outer operand must be SSA-identical to result dimension $idx")
        else if !sameIndexValue(inner, res.typ.params(idx + 1)) then
          Err(s"d_tensor.split_dim: inner operand must be SSA-identical to result dimension ${idx + 1}")
        else OK(this)

final case class JoinDim(
    src: Operand[DTensorTensorType],
    dim: IntegerAttr,
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.join_dim"]
    with NoMemoryEffect derives OpDefs:

  private def sameDim(lhs: DimParam, rhs: DimParam): Boolean =
    DTensorTypeUtil.sameDims(Seq(lhs), Seq(rhs))

  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_tensor.join_dim: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if resRank != srcRank - 1 then
      Err(
        s"d_tensor.join_dim: expected result rank = input rank - 1, got $srcRank -> $resRank"
      )
    else if dim.typ != I32 then
      Err(s"d_tensor.join_dim: expected i32 dim attribute, got ${dim.typ}")
    else
      val axis = dim.value.value
      if axis < 0 || axis + 1 >= srcRank then
        Err(s"d_tensor.join_dim: dim $axis out of bounds for rank $srcRank")
      else
        val idx = axis.toInt
        val prefixOk = (0 until idx).forall(i => sameDim(src.typ.params(i), res.typ.params(i)))
        val suffixOk = ((idx + 2) until srcRank).forall(i =>
          sameDim(src.typ.params(i), res.typ.params(i - 1))
        )
        if !prefixOk then
          Err("d_tensor.join_dim: expected dimensions before joined dim to be SSA-identical")
        else if !suffixOk then
          Err("d_tensor.join_dim: expected dimensions after joined dim to be shifted and SSA-identical")
        else OK(this)

final case class PermuteDims(
    src: Operand[DTensorTensorType],
    permutation: ArrayAttribute[Attribute],
    res: Result[DTensorTensorType],
) extends DerivedOperation["d_tensor.permute_dims"]
    with NoMemoryEffect derives OpDefs:

  private def parsePermutation: OK[Seq[Int]] =
    permutation.attrValues.zipWithIndex
      .foldLeft[OK[Seq[Int]]](OK(Seq.empty)) {
        case (acc, (idxAttr, idxPos)) =>
          acc.flatMap(indices =>
            idxAttr match
              case IntegerAttr(IntData(idx), I32) =>
                OK(indices :+ idx.toInt)
              case other =>
                Err(
                  s"d_tensor.permute_dims: permutation index $idxPos must be an i32 integer attribute, got ${DTensorTypeUtil.renderAttr(other)}"
                )
          )
      }

  private def checkPermutation(perm: Seq[Int], rank: Int): OK[Unit] =
    if perm.size != rank then
      Err(
        s"d_tensor.permute_dims: expected permutation length $rank, got ${perm.size}"
      )
    else if perm.exists(_ < 0) then
      Err("d_tensor.permute_dims: permutation entries must be non-negative")
    else if perm.exists(_ >= rank) then
      Err(s"d_tensor.permute_dims: permutation entries must be less than rank $rank")
    else if perm.distinct.size != perm.size then
      Err("d_tensor.permute_dims: permutation entries must be unique")
    else OK(())

  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_tensor.permute_dims: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if srcRank != resRank then
      Err(
        s"d_tensor.permute_dims: expected equal ranks, got $srcRank and $resRank"
      )
    else
      parsePermutation.flatMap(perm =>
        checkPermutation(perm, srcRank).flatMap(_ =>
          val dimsOk = perm.zipWithIndex.forall { case (srcIdx, resIdx) =>
            DTensorTypeUtil.sameDims(
              Seq(src.typ.params(srcIdx)),
              Seq(res.typ.params(resIdx)),
            )
          }
          if dimsOk then OK(this)
          else Err("d_tensor.permute_dims: expected output dims to match the declared permutation")
        )
      )
