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

  inline def tvar(tparam: Value[Attribute]): TlamTVarType =
    TlamTVarType(ValueAttribute(tparam))

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

  def tlam(resTy: TlamForAllType)(
      body: Value[Attribute] => Seq[Operation]
  ): TLambda =
    val res = Result[TlamForAllType](resTy)
    val region =
      Region(
        Seq(
          Block(
            TlamTypeType(),
            (t: Value[Attribute]) => body(t),
          )
        )
      )
    TLambda(body = region, res = res)

  def polyIdDef(): TLambda =
    val forallTy: TlamForAllType = forall1(alphaToAlphaAt(0))

    tlam(forallTy) { (T: Value[Attribute]) =>
      val inOut: TypeAttribute = tvar(T)
      val idBodyTy: TlamFunType = fun(inOut, inOut)

      val vId = vlam(idBodyTy)(inOut)(x => Seq(VReturn(x)))
      Seq(vId, TReturn(vId.res))
    }
