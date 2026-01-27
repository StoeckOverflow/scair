package scair.testutils.tlam

import scair.ir.*
import scair.dialects.tlam.*
import scair.dialects.tlam.TlamTy.*
import scair.dialects.builtin.*

object TlamTestIR:

  // --- tiny constructors / aliases ---
  inline def i(n: Int): IntData = IntData(n)
  inline def b(n: Int): TlamBVarType = bvar(i(n))
  inline def b0: TlamBVarType = b(0)
  inline def b1: TlamBVarType = b(1)
  inline def b2: TlamBVarType = b(2)

  inline def alphaToAlphaAt(idx: Int): TlamFunType = fun(b(idx), b(idx))
  inline def forall1(body: TypeAttribute): TlamForAllType = forall(body)

  // --- common IR building patterns ---
  def module(ops: Operation*): ModuleOp =
    ModuleOp(Region(Seq(Block(operations = ops.toSeq))))

  def vlam(funTy: TlamFunType)(argTy: TypeAttribute)(
      bodyOps: Value[TypeAttribute] => Seq[Operation]
  ): VLambda =
    val res = Result[TlamFunType](funTy)
    val region =
      Region(
        Seq(
          Block(
            argTy,
            (x: Value[Attribute]) =>
              bodyOps(x.asInstanceOf[Value[TypeAttribute]]),
          )
        )
      )
    VLambda(body = region, res = res)

  def tlam(resTy: TlamForAllType)(ops: Operation*): TLambda =
    val res = Result[TlamForAllType](resTy)
    val region = Region(Seq(Block(operations = ops.toSeq)))
    TLambda(body = region, res = res)

  def polyIdDef(): TLambda =
    val idBodyTy = fun(b0, b0)
    val idPolyTy: TlamForAllType = forall1(idBodyTy)

    val vId = vlam(idBodyTy)(b0)(x => Seq(VReturn(x)))

    tlam(idPolyTy)(vId, TReturn(vId.res))
