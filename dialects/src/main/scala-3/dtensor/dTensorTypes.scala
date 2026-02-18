package scair.dialects.dTensor

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.ir.*
import scair.parse.*
import scair.utils.*

type DimParam = ValueAttribute

sealed trait dTensorType extends ParametrizedAttribute, TypeAttribute

final case class dTensorNatType()
    extends TypeAttribute
    with DerivedAttribute["dtensor.nat", dTensorNatType]
    derives DerivedAttributeCompanion

/** Surface-shape aliases:
  *   - vector is dTensor rank-1
  *   - matrix is dTensor rank-2
  */
final case class dTensorVectorType(param: DimParam, elem: TypeAttribute)
    extends dTensorType:
  override def name: String = "dtensor.vector"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(param, elem)

  override def customVerify(): OK[Unit] =
    dTensorTypeUtil.checkParam(param).flatMap(_ =>
      if dTensorTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid dtensor.vector element type `${dTensorTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[dTensorVectorType]:
  override def name: String = "dtensor.vector"

  override def parse[$: P](using Parser): P[dTensorVectorType] =
    P("<" ~ ValueAttributeP ~ "," ~ typeP ~ ">").map((param, elem) =>
      dTensorVectorType(param, elem.asInstanceOf[TypeAttribute])
    )

final case class dTensorMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
) extends dTensorType:
  override def name: String = "dtensor.matrix"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(rows, cols, elem)

  override def customVerify(): OK[Unit] =
    dTensorTypeUtil.checkParam(rows).flatMap(_ =>
      dTensorTypeUtil.checkParam(cols)
    ).flatMap(_ =>
      if dTensorTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid dtensor.matrix element type `${dTensorTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[dTensorMatrixType]:
  override def name: String = "dtensor.matrix"

  override def parse[$: P](using Parser): P[dTensorMatrixType] =
    P("<" ~ ValueAttributeP ~ "," ~ ValueAttributeP ~ "," ~ typeP ~ ">")
      .map((rows, cols, elem) =>
        dTensorMatrixType(rows, cols, elem.asInstanceOf[TypeAttribute])
      )

final case class dTensorTensorType(
    params: Seq[ValueAttribute],
    elem: TypeAttribute,
) extends dTensorType
    with ParametrizedAttribute:
  override def name: String = "dtensor.tensor"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(params, elem)

  override def printParameters(p: Printer): Unit =
    p.print("<[")
    p.printListF(params, p.print, sep = ", ")
    p.print("], ", elem, ">")(using indentLevel = 0)

  override def customVerify(): OK[Unit] =
    params.foldLeft[OK[Unit]](OK(()))((acc, p) =>
      acc.flatMap(_ => dTensorTypeUtil.checkParam(p))
    ).flatMap(_ =>
      if dTensorTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid dtensor element type `${dTensorTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[dTensorTensorType]:
  override def name: String = "dtensor.tensor"

  override def parse[$: P](using Parser): P[dTensorTensorType] =
    P("<" ~ "[" ~ ValueAttributeP.rep(sep = ",") ~ "]" ~ "," ~ typeP ~ ">")
      .map((params, elem) =>
        dTensorTensorType(params, elem.asInstanceOf[TypeAttribute])
      )
