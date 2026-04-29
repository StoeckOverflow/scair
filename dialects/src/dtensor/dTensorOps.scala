package scair.dialects.dTensor

import scair.print.Printer
import scair.clair.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

final case class NatParam(
    res: Result[dTensorNatType]
) extends DerivedOperation["dtensor.nat.param"] derives OpDefs

final case class NatConst(
    value: IntegerAttr,
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.nat.const"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if value.value.value >= 0 then OK(this)
    else Err(s"dtensor.nat.const: expected non-negative literal, got $value")

final case class NatAdd(
    lhs: Operand[dTensorNatType],
    rhs: Operand[dTensorNatType],
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.nat.add"]
    with NoMemoryEffect derives OpDefs

final case class NatMul(
    lhs: Operand[dTensorNatType],
    rhs: Operand[dTensorNatType],
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.nat.mul"]
    with NoMemoryEffect derives OpDefs

final case class ShapeToIndex(
    nat: Operand[dTensorNatType],
    res: Result[IndexType],
) extends DerivedOperation["dtensor.shape.to_index"]
    with NoMemoryEffect derives OpDefs

final case class IndexToNat(
    index: Operand[IndexType],
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.index_to_nat"]
    with NoMemoryEffect derives OpDefs

final case class Empty(
    res: Result[dTensorTensorType]
) extends DerivedOperation["dtensor.empty"]
    with NoMemoryEffect derives OpDefs

final case class Fill(
    v: Operand[TypeAttribute],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.fill"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if v.typ == res.typ.elem then OK(this)
    else
      Err(
        s"dtensor.fill: expected fill value type ${res.typ.elem}, got ${v.typ}"
      )

final case class Dim(
    t: Operand[dTensorTensorType],
    axis: IntegerAttr,
    res: Result[ValueRefType],
) extends DerivedOperation["dtensor.dim"]
    with NoMemoryEffect derives OpDefs:

  def selectedDimValue: OK[Value[Attribute]] =
    val idx = axis.value.value
    val rank = BigInt(t.typ.params.size)
    if idx < 0 || idx >= rank then
      Err(s"dtensor.dim: axis $idx out of bounds for rank ${t.typ.params.size}")
    else OK(t.typ.params(idx.toInt).getVal())

  override def customVerify(): OK[Operation] =
    val axisTyOk = axis.typ == I32
    if !axisTyOk then
      Err(s"dtensor.dim: expected i32 axis attribute, got ${axis.typ}")
    else
      selectedDimValue.flatMap(sel =>
        if res.typ.ref.getVal() eq sel then
          dTensorTypeUtil.resolveNatValue(res.typ.ref.getVal()).map(_ => this)
        else
          Err(
            "dtensor.dim: expected result !value<...> to reference the selected embedded dim"
          )
      )

final case class Add(
    lhs: Operand[dTensorTensorType],
    rhs: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.add"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    dTensorTypeUtil
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "dtensor.add")
      .map(_ => this)

final case class Mul(
    lhs: Operand[dTensorTensorType],
    rhs: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.mul"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    dTensorTypeUtil
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "dtensor.mul")
      .map(_ => this)

final case class Matmul(
    lhs: Operand[dTensorTensorType],
    rhs: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.matmul"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    dTensorTypeUtil.checkMatmul(lhs.typ, rhs.typ, res.typ).map(_ => this)

final case class Cast(
    src: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.cast"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"dtensor.cast: expected equal element types, got ${src.typ
            .elem} and ${res.typ.elem}"
      )
    else if src.typ.params.size != res.typ.params.size then
      Err(
        s"dtensor.cast: expected equal ranks, got ${src.typ.params
            .size} and ${res.typ.params.size}"
      )
    else if !dTensorTypeUtil.sameDims(src.typ.params, res.typ.params)
    then Err("dtensor.cast: expected pairwise SSA-identical dims")
    else OK(this)

final case class ExpandShape(
    src: Operand[dTensorTensorType],
    reassociation: ArrayAttribute[Attribute],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.expand_shape"]
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
                              s"dtensor.expand_shape: reassociation group $groupIdx index $idxPos must be an i32 integer attribute, got ${dTensorTypeUtil.renderAttr(other)}"
                            )
                      )
                  }
                  .map(groupIndices => groups :+ groupIndices)
              case other =>
                Err(
                  s"dtensor.expand_shape: reassociation group $groupIdx must be an array attribute, got ${dTensorTypeUtil.renderAttr(other)}"
                )
          )
      }

  private def checkReassociation(
      groups: Seq[Seq[Int]],
      srcRank: Int,
      resRank: Int,
  ): OK[Int] =
    if groups.size != srcRank then
      Err(
        s"dtensor.expand_shape: expected $srcRank reassociation groups, got ${groups.size}"
      )
    else if groups.exists(_.isEmpty) then
      Err("dtensor.expand_shape: reassociation groups must be non-empty")
    else if groups.exists(_.exists(_ < 0)) then
      Err("dtensor.expand_shape: reassociation indices must be non-negative")
    else
      val flattened = groups.flatten
      val expected = 0 until resRank
      if flattened != expected then
        Err(
          s"dtensor.expand_shape: reassociation must cover result dims contiguously as ${expected.mkString("[", ", ", "]")}"
        )
      else
        val badGroup = groups.collectFirst {
          case group if group.size != 1 && group.size != 2 => group
        }
        badGroup match
          case Some(group) =>
            Err(
              s"dtensor.expand_shape: v1 supports only singleton groups and one 2-dim split, got ${group.mkString("[", ", ", "]")}"
            )
          case None =>
            val splitGroups = groups.zipWithIndex.collect {
              case (group, idx) if group.size == 2 => idx
            }
            splitGroups match
              case Seq(splitIdx) => OK(splitIdx)
              case _ =>
                Err(
                  s"dtensor.expand_shape: v1 expects exactly one source dim split into two result dims, got ${splitGroups.size}"
                )

  private def sameDim(lhs: Value[Attribute], rhs: ValueAttribute): Boolean =
    dTensorTypeUtil.sameDims(Seq(ValueAttribute(lhs)), Seq(rhs))

  private def verifySplit(
      groups: Seq[Seq[Int]],
      splitSrcIdx: Int,
  ): OK[Operation] =
    groups.zipWithIndex
      .foldLeft[OK[Unit]](OK(())) { case (acc, (group, srcIdx)) =>
        acc.flatMap(_ =>
          if srcIdx == splitSrcIdx then OK(())
          else
            val resIdx = group.head
            if dTensorTypeUtil.sameDims(
                Seq(src.typ.params(srcIdx)),
                Seq(res.typ.params(resIdx)),
              )
            then OK(())
            else
              Err(
                s"dtensor.expand_shape: expected unchanged dim $srcIdx to be SSA-identical to result dim $resIdx"
              )
        )
      }
      .flatMap(_ =>
        dTensorTypeUtil.resolveNatValue(src.typ.params(splitSrcIdx).getVal())
      )
      .flatMap(splitNat =>
        splitNat.owner match
          case Some(NatMul(lhs, rhs, _)) =>
            val Seq(lhsResIdx, rhsResIdx) = groups(splitSrcIdx)
            if !sameDim(lhs, res.typ.params(lhsResIdx)) then
              Err(
                "dtensor.expand_shape: split lhs must match the first result split dim"
              )
            else if !sameDim(rhs, res.typ.params(rhsResIdx)) then
              Err(
                "dtensor.expand_shape: split rhs must match the second result split dim"
              )
            else OK(this)
          case _ =>
            Err(
              "dtensor.expand_shape: split source dim must be produced by direct dtensor.nat.mul"
            )
      )

  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if src.typ.elem != res.typ.elem then
      Err(
        s"dtensor.expand_shape: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if resRank != srcRank + 1 then
      Err(
        s"dtensor.expand_shape: v1 expected result rank = source rank + 1, got $srcRank -> $resRank"
      )
    else
      parseReassociationGroups.flatMap(groups =>
        checkReassociation(groups, srcRank, resRank).flatMap(splitSrcIdx =>
          verifySplit(groups, splitSrcIdx)
        )
      )
