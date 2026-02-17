package scair.dialects.tensor

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

sealed trait TensorType extends TypeAttribute
type DimParam = ValueAttribute

final case class TensorNatType()
    extends TypeAttribute
    with DerivedAttribute["tensor.nat", TensorNatType]
    derives DerivedAttributeCompanion

/** Surface-shape aliases:
  *   - vector is tensor rank-1
  *   - matrix is tensor rank-2
  *
  * Keeping explicit vector/matrix syntax enables future lowering choices (e.g.
  * SIMD-friendly vector lowering) while still allowing canonical tensor-based
  * handling in shared passes.
  */
final case class TensorVectorType(param: DimParam, elem: TypeAttribute)
    extends TensorType
    with ParametrizedAttribute:
  override def name: String = "tensor.vector"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(param, elem)
  override def customVerify(): OK[Unit] = TensorTypeVerify.checkVector(this)

final case class TensorMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
) extends TensorType
    with ParametrizedAttribute:
  override def name: String = "tensor.matrix"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(rows, cols, elem)

  override def customVerify(): OK[Unit] = TensorTypeVerify.checkMatrix(this)

final case class TensorTensorType(params: Seq[DimParam], elem: TypeAttribute)
    extends TensorType
    with ParametrizedAttribute:
  override def name: String = "tensor.tensor"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(params, elem)

  override def printParameters(p: Printer): Unit =
    p.print("<[")
    p.printListF(params, p.print, sep = ", ")
    p.print("], ", elem, ">")(using indentLevel = 0)

  override def customVerify(): OK[Unit] = TensorTypeVerify.checkTensor(this)

object TensorTypeUtil:

  /** Canonical tensor view for lowering/passes. vector -> rank-1 tensor matrix
    * -> rank-2 tensor
    */
  def asTensor(t: TensorType): TensorTensorType =
    t match
      case TensorVectorType(param, elem) =>
        TensorTensorType(Seq(param), elem)
      case TensorMatrixType(rows, cols, elem) =>
        TensorTensorType(Seq(rows, cols), elem)
      case tt: TensorTensorType =>
        tt

private object TensorTypeVerify:

  private def renderAttr(a: Attribute): String =
    val out = java.io.StringWriter()
    val printer = Printer(p = java.io.PrintWriter(out))
    printer.print(a)
    printer.flush()
    out.toString

  private def checkParam(param: DimParam): OK[Unit] =
    param match
      case va: ValueAttribute =>
        va.getVal().typ match
          case _: TensorNatType => OK(())
          case other            =>
            Err(
              s"shape SSA parameter must have type !tensor.nat, got ${renderAttr(other)}"
            )

  private def elemOK(elem: TypeAttribute): Boolean =
    elem match
      case _: IntegerType => true
      case _: FloatType   => true
      case _              => false

  def checkVector(t: TensorVectorType): OK[Unit] =
    checkParam(t.param).flatMap(_ =>
      if elemOK(t.elem) then OK(())
      else Err(s"invalid vector element type `${renderAttr(t.elem)}`")
    )

  def checkMatrix(t: TensorMatrixType): OK[Unit] =
    checkParam(t.rows).flatMap(_ => checkParam(t.cols)).flatMap(_ =>
      if elemOK(t.elem) then OK(())
      else Err(s"invalid matrix element type `${renderAttr(t.elem)}`")
    )

  def checkTensor(t: TensorTensorType): OK[Unit] =
    t.params
      .foldLeft[OK[Unit]](OK(()))((acc, p) => acc.flatMap(_ => checkParam(p)))
      .flatMap(_ =>
        if elemOK(t.elem) then OK(())
        else Err(s"invalid tensor element type `${renderAttr(t.elem)}`")
      )

private def dimParamP[$: P](using p: Parser): P[DimParam] = P(
  operandNameP.flatMap(existingOperandP).map(v => ValueAttribute(v))
)

given AttributeCompanion[TensorVectorType]:
  override def name: String = "tensor.vector"

  override def parse[$: P](using Parser): P[TensorVectorType] =
    P("<" ~ dimParamP ~ "," ~ typeP ~ ">").map((param, elem) =>
      TensorVectorType(param, elem.asInstanceOf[TypeAttribute])
    )

given AttributeCompanion[TensorMatrixType]:
  override def name: String = "tensor.matrix"

  override def parse[$: P](using Parser): P[TensorMatrixType] =
    P("<" ~ dimParamP ~ "," ~ dimParamP ~ "," ~ typeP ~ ">")
      .map((rows, cols, elem) =>
        TensorMatrixType(rows, cols, elem.asInstanceOf[TypeAttribute])
      )

given AttributeCompanion[TensorTensorType]:
  override def name: String = "tensor.tensor"

  override def parse[$: P](using Parser): P[TensorTensorType] =
    P("<" ~ "[" ~ dimParamP.rep(sep = ",") ~ "]" ~ "," ~ typeP ~ ">")
      .map((params, elem) =>
        TensorTensorType(params, elem.asInstanceOf[TypeAttribute])
      )
