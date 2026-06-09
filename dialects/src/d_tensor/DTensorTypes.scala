package scair.dialects.d_tensor

import fastparse.*
import scair.print.Printer
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*
import scala.language.implicitConversions

type DimParam = ValueAttribute | IntegerAttr

extension (param: DimParam)
  def getVal(): Value[Attribute] =
    param match
      case v: ValueAttribute => v.getVal()
      case i: IntegerAttr =>
        throw new IllegalArgumentException(
          s"static d_tensor dimension ${DTensorTypeUtil.renderAttr(i)} is not an SSA value"
        )

given Conversion[Seq[DimParam], Seq[ValueAttribute]] with
  def apply(params: Seq[DimParam]): Seq[ValueAttribute] =
    params.map {
      case v: ValueAttribute => v
      case i: IntegerAttr =>
        throw new IllegalArgumentException(
          s"static d_tensor dimension ${DTensorTypeUtil.renderAttr(i)} is not an SSA value"
        )
    }

sealed trait DTensorType extends ParametrizedAttribute, TypeAttribute

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
      parameters(0).asInstanceOf[DimParam],
      parameters(1).asInstanceOf[TypeAttribute],
    )

  override def printParameters(p: Printer): Unit =
    p.print("<")
    DTensorTypeUtil.printDimParam(p, param)
    p.print(", ", elem, ">")

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
    P("<" ~ DimParamP ~ "," ~ typeP ~ ">").map((param, elem) =>
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
      parameters(0).asInstanceOf[DimParam],
      parameters(1).asInstanceOf[DimParam],
      parameters(2).asInstanceOf[TypeAttribute],
    )

  override def printParameters(p: Printer): Unit =
    p.print("<")
    DTensorTypeUtil.printDimParam(p, rows)
    p.print(", ")
    DTensorTypeUtil.printDimParam(p, cols)
    p.print(", ", elem, ">")

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
    P("<" ~ DimParamP ~ "," ~ DimParamP ~ "," ~ typeP ~ ">")
      .map((rows, cols, elem) =>
        DTensorMatrixType(rows, cols, elem.asInstanceOf[TypeAttribute])
      )

final case class DTensorTensorType(
    params: Seq[DimParam],
    elem: TypeAttribute,
) extends DTensorType
    with ParametrizedAttribute:
  override def name: String = "d_tensor.tensor"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(params, elem)
  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    DTensorTensorType(
      parameters(0).asInstanceOf[Seq[DimParam]],
      parameters(1).asInstanceOf[TypeAttribute],
    )

  override def printParameters(p: Printer): Unit =
    p.print("<[")
    p.printListF(params, DTensorTypeUtil.printDimParam(p, _), sep = ", ")
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
    P("<" ~ "[" ~ DimParamP.rep(sep = ",") ~ "]" ~ "," ~ typeP ~ ">")
      .map((params, elem) =>
        DTensorTensorType(params, elem.asInstanceOf[TypeAttribute])
      )

def ValueAttributeP[$: P](using p: Parser): P[ValueAttribute] =
  operandNameP.flatMap(existingOrForwardValueRefOperandP)

def DimParamP[$: P](using Parser): P[DimParam] =
  P(
    ValueAttributeP.map(v => v: DimParam) |
      attrOfP[IntegerAttr].map(i => i: DimParam) |
      decimalLiteralP.map(v => IntegerAttr(IntData(v), IndexType()): DimParam)
  )
