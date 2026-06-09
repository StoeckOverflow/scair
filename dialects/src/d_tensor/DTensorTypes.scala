package scair.dialects.d_tensor

import fastparse.*
import scair.print.Printer
import scair.clair.*
import scair.ir.*
import scair.parse.*
import scair.utils.*

type DimParam = ValueAttribute

sealed trait DTensorType extends ParametrizedAttribute, TypeAttribute
sealed trait DTensorNatLikeType extends TypeAttribute

final case class DTensorNatType()
    extends DTensorNatLikeType
    with DerivedAttribute["d_tensor.nat"]
    derives AttrDefs

final case class DTensorPosNatType()
    extends DTensorNatLikeType
    with DerivedAttribute["d_tensor.posnat"]
    derives AttrDefs

/** Surface-shape aliases:
  *   - vector is DTensor rank-1
  *   - matrix is DTensor rank-2
  */
final case class DTensorVectorType(param: DimParam, elem: TypeAttribute)
    extends DTensorType:
  override def name: String = "d_tensor.vector"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(param, elem)
  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    DTensorVectorType(
      parameters(0).asInstanceOf[ValueAttribute],
      parameters(1).asInstanceOf[TypeAttribute],
    )

  override def customVerify(): OK[Unit] =
    DTensorTypeUtil.checkParam(param).flatMap(_ =>
      if DTensorTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_tensor.vector element type `${DTensorTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[DTensorVectorType]:
  override def name: String = "d_tensor.vector"

  override def parse[$: P](using Parser): P[DTensorVectorType] =
    P("<" ~ ValueAttributeP ~ "," ~ typeP ~ ">").map((param, elem) =>
      DTensorVectorType(param, elem.asInstanceOf[TypeAttribute])
    )

final case class DTensorMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
) extends DTensorType:
  override def name: String = "d_tensor.matrix"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(rows, cols, elem)
  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    DTensorMatrixType(
      parameters(0).asInstanceOf[ValueAttribute],
      parameters(1).asInstanceOf[ValueAttribute],
      parameters(2).asInstanceOf[TypeAttribute],
    )

  override def customVerify(): OK[Unit] =
    DTensorTypeUtil.checkParam(rows).flatMap(_ =>
      DTensorTypeUtil.checkParam(cols)
    ).flatMap(_ =>
      if DTensorTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_tensor.matrix element type `${DTensorTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[DTensorMatrixType]:
  override def name: String = "d_tensor.matrix"

  override def parse[$: P](using Parser): P[DTensorMatrixType] =
    P("<" ~ ValueAttributeP ~ "," ~ ValueAttributeP ~ "," ~ typeP ~ ">")
      .map((rows, cols, elem) =>
        DTensorMatrixType(rows, cols, elem.asInstanceOf[TypeAttribute])
      )

final case class DTensorTensorType(
    params: Seq[ValueAttribute],
    elem: TypeAttribute,
) extends DTensorType
    with ParametrizedAttribute:
  override def name: String = "d_tensor.tensor"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(params, elem)
  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    DTensorTensorType(
      parameters(0).asInstanceOf[Seq[ValueAttribute]],
      parameters(1).asInstanceOf[TypeAttribute],
    )

  override def printParameters(p: Printer): Unit =
    p.print("<[")
    p.printListF(params, p.print, sep = ", ")
    p.print("], ", elem, ">")

  override def customVerify(): OK[Unit] =
    params.foldLeft[OK[Unit]](OK(()))((acc, p) =>
      acc.flatMap(_ => DTensorTypeUtil.checkParam(p))
    ).flatMap(_ =>
      if DTensorTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_tensor element type `${DTensorTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[DTensorTensorType]:
  override def name: String = "d_tensor.tensor"

  override def parse[$: P](using Parser): P[DTensorTensorType] =
    P("<" ~ "[" ~ ValueAttributeP.rep(sep = ",") ~ "]" ~ "," ~ typeP ~ ">")
      .map((params, elem) =>
        DTensorTensorType(params, elem.asInstanceOf[TypeAttribute])
      )

def ValueAttributeP[$: P](using p: Parser): P[ValueAttribute] =
  import scair.parse.given
  operandNameP.flatMap(existingOrForwardValueRefOperandP)
