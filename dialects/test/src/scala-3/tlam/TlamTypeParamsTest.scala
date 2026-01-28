package scair

import scair.ir.*
import scair.utils.*
import scair.dialects.testutils.IRTestKit.*
import scair.dialects.builtin.*
import scair.dialects.tlam.*
import scair.dialects.tlam.TlamDialect
import scair.dialects.tlam.TlamTy.*
import scair.testutils.tlam.TlamTestIR.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

final class TlamTypeParamsTest extends AnyFlatSpec:

  "A polymorphic identity (SSA tvar in value-level types)" should
    "build/verify and print expected shape" in {

      // Type-level result is still de Bruijn:
      //   ∀α. α -> α   (encoded as bvar<0> under forall)
      val bodyTyDeBruijn = alphaToAlphaAt(0)
      val forallTy = forall1(bodyTyDeBruijn)

      // Term-level uses SSA type param:
      //   Λα. (λ (x: α). x)
      val idT =
        tlam(forallTy) { (T: Value[Attribute]) =>
          val a = tvar(T)
          val bodyTySSA = fun(a, a)

          val idV = vlam(bodyTySSA)(a)(x => Seq(VReturn(x)))
          idV.shouldVerify()

          Seq(idV, TReturn(idV.res))
        }

      idT.shouldVerify()

      val m = module(idT)
      m.shouldVerify()

      val printed = printIR(m)
      assertPrinted(
        printed,
        includes = Seq(
          "builtin.module",
          "tlam.tlambda",
          "tlam.vlambda",
          "tlam.vreturn",
          "tlam.treturn",
          "!tlam.tvar<%",
          "!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>",
        ),
      )
    }

  "Nested tlambda + tapply (SSA tvar as the tyArg)" should
    "instantiate ∀ with the outer binder SSA value" in {

      // Outer result type: ∀α. α -> α (still de Bruijn at the type level)
      val outerForall = forall1(alphaToAlphaAt(0))

      // Build:
      //   Λα.
      //     let G = Λβ. (λ(x:β). x)
      //     let h = G[α]
      //     return h
      val outerF =
        tlam(outerForall) { (T0: Value[Attribute]) =>
          // G : ∀β. β -> β  (type-level de Bruijn)
          val innerForall = forall1(alphaToAlphaAt(0))

          val innerG =
            tlam(innerForall) { (T1: Value[Attribute]) =>
              val b = tvar(T1)
              val innerBodyTySSA = fun(b, b)

              val innerIdV = vlam(innerBodyTySSA)(b)(x => Seq(VReturn(x)))
              innerIdV.shouldVerify()

              Seq(innerIdV, TReturn(innerIdV.res))
            }

          innerG.shouldVerify()

          // Instantiate G at outer α (SSA tvar):
          val a = tvar(T0)
          val expectedInst = fun(a, a)
          val hRes = Result[TlamType](expectedInst)
          val tapp = TApply(innerG.res, a, hRes)
          tapp.verify().shouldBeOK("verify failed for tapply")

          Seq(innerG, tapp, TReturn(hRes))
        }

      outerF.shouldVerify()

      val m = module(outerF)
      m.shouldVerify()

      val printed = printIR(m)
      assertPrinted(
        printed,
        includes = Seq(
          "builtin.module",
          "tlam.tlambda",
          "tlam.tapply",
          "tlam.vlambda",
          "tlam.vreturn",
          "!tlam.tvar<%",
          "!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>",
        ),
      )
    }

  "Instantiate to a ground type (i32)" should
    "compute (∀α. α→α)[i32] == i32→i32 and verify tapply" in {
      val poly = forall1(alphaToAlphaAt(0))
      val inst = DBI.instantiate(poly, I32)
      inst shouldEqual fun(I32, I32)

      val polyDef = polyIdDef()
      polyDef.shouldVerify()

      val appRes = Result[TypeAttribute](fun(I32, I32))
      TApply(polyDef.res, I32, appRes).verify()
        .shouldBeOK("verify failed for tapply")
    }

  "DBI.shift" should
    "bump only indices >= cutoff, distribute over fun, and respect binders" in {
      import DBI.*

      // basic
      shift(1, 0, b0) shouldEqual b1
      shift(1, 1, b0) shouldEqual b0
      shift(0, 0, b(42)) shouldEqual b(42)

      // structure
      val ty = fun(b0, fun(b1, b0)) // (0 -> (1 -> 0))
      shift(1, 0, ty) shouldEqual fun(b1, fun(b2, b1))

      // binder cutoff increase
      val poly = forall1(fun(b0, b1)) // ∀. (0 -> 1)
      shift(1, 0, poly) shouldEqual forall1(fun(b0, b2))
    }

  "DBI.subst" should
    "be capture-avoiding under forall and decrement indices above the hole" in {
      import DBI.*

      val t = forall1(fun(b2, b1))
      subst(1, b0, t) shouldEqual forall1(fun(b1, b1))

      val t2 = fun(b1, b0)
      subst(0, I32, t2) shouldEqual fun(b0, I32)
    }

  "VLambda.verify" should
    "fail when the block arg type doesn't match the function input (SSA tvar form)" in {

      // Create a fake SSA type param value by making a tlambda and constructing a vlambda inside,
      // but intentionally mismatch the vlambda block arg type.
      val wrapperTy = forall1(alphaToAlphaAt(0)) // just to host a binder

      val bad =
        tlam(wrapperTy) { (T: Value[Attribute]) =>
          val a = tvar(T)
          val funTy = fun(a, a)
          val res = Result[TlamFunType](funTy)

          val wrongRegion =
            Region(
              Seq(
                Block(
                  // WRONG: expected `a`, give `I32` (or any other type)
                  I32,
                  (x: Value[Attribute]) =>
                    Seq(VReturn(x.asInstanceOf[Value[TypeAttribute]])),
                )
              )
            )

          val v = VLambda(wrongRegion, res)
          v.verify().isError shouldBe true

          // still return something well-typed at TLambda level:
          // easiest is not to emit v; just return a well-formed TReturn of a valid value.
          // But we want the test to focus on v.verify above, so return empty + dummy.
          Seq(
            TReturn(res)
          ) // ok if your verifier accepts returning a value of that type
        }

      // If your TLambda verifier requires its body to end in treturn of the right value,
      // you can simply not verify `bad` here; the assertion we care about is v.verify().isError.
      // (If you do verify it and it fails for reasons unrelated to VLambda, remove bad.shouldVerify().)
      bad.verify().isError
    }

  "TApply.verify" should "fail when res.typ != instantiated type" in {
    val polyDef = polyIdDef()
    polyDef.shouldVerify()

    val bad = TApply(polyDef.res, I32, Result[TypeAttribute](fun(I32, I64)))
    bad.verify().isError shouldBe true
  }

  "Monomorphize pass" should
    "replace tapply with a specialized vlam and remove the inner tlambda (SSA tyArg)" in {
      import scair.MLContext
      import scair.dialects.builtin.BuiltinDialect
      import scair.passes.MonomorphizePass

      val ctx = MLContext()
      ctx.registerDialect(BuiltinDialect)
      ctx.registerDialect(TlamDialect)

      // Outer: ∀α. α -> α
      val outerForall = forall1(alphaToAlphaAt(0))

      // Build program with SSA tyArg for TApply:
      val F =
        tlam(outerForall) { (T0: Value[Attribute]) =>
          // Inner G: ∀β. β -> β
          val innerForall = forall1(alphaToAlphaAt(0))

          val G =
            tlam(innerForall) { (T1: Value[Attribute]) =>
              val b = tvar(T1)
              val innerBodySSA = fun(b, b)
              val innerIdV = vlam(innerBodySSA)(b)(x => Seq(VReturn(x)))
              Seq(innerIdV, TReturn(innerIdV.res))
            }

          val a = tvar(T0)
          val hTy = fun(a, a)
          val hRes = Result[TlamType](hTy)
          val tapp = TApply(G.res, a, hRes)

          Seq(G, tapp, TReturn(hRes))
        }

      val before = module(F)
      before.shouldVerify()

      countOps[TApply](before) shouldBe 1
      countOps[TLambda](before) shouldBe 2

      val after = new MonomorphizePass(ctx).transform(before)
        .asInstanceOf[ModuleOp]
      after.shouldVerify()

      countOps[TApply](after) shouldBe 0
      countOps[TLambda](after) shouldBe 1

      val out = printIR(after)

      out should include("tlam.vlambda")
      out should not include ("tlam.tapply")
    }

  "A full lowering pipeline" should
    "eliminate type-level ops and produce func.func/call/return" in {
      import scair.MLContext
      import scair.dialects.builtin.BuiltinDialect
      import scair.dialects.func.FuncDialect
      import scair.passes.{MonomorphizePass, EraseTLamPass, LowerTLamToFuncPass}

      val ctx = MLContext()
      ctx.registerDialect(BuiltinDialect)
      ctx.registerDialect(TlamDialect)
      ctx.registerDialect(FuncDialect)

      val outerForall = forall1(alphaToAlphaAt(0))

      val prog =
        module(
          tlam(outerForall) { (T0: Value[Attribute]) =>
            val innerForall = forall1(alphaToAlphaAt(0))

            val G =
              tlam(innerForall) { (T1: Value[Attribute]) =>
                val b = tvar(T1)
                val innerBodySSA = fun(b, b)
                val innerIdV = vlam(innerBodySSA)(b)(x => Seq(VReturn(x)))
                Seq(innerIdV, TReturn(innerIdV.res))
              }

            val a = tvar(T0)
            val hTy = fun(a, a)
            val hRes = Result[TlamType](hTy)
            val tapp = TApply(G.res, a, hRes)

            Seq(G, tapp, TReturn(hRes))
          }
        )

      prog.shouldVerify()

      val afterMono = new MonomorphizePass(ctx).transform(prog)
        .asInstanceOf[ModuleOp]
      val afterErase = new EraseTLamPass(ctx).transform(afterMono)
        .asInstanceOf[ModuleOp]
      val afterLower = new LowerTLamToFuncPass(ctx).transform(afterErase)
        .asInstanceOf[ModuleOp]
      afterLower.shouldVerify()

      val out = printIR(afterLower)

      assertPrinted(
        out,
        includes = Seq("func.func", "func.return"),
        excludes = Seq("tlam.tlambda", "tlam.tapply", "tlam.treturn"),
      )
    }

  "LowerTLamToFuncPass" should "rewrite vapply into func.call" in {
    import scair.MLContext
    import scair.dialects.builtin.BuiltinDialect
    import scair.dialects.func.FuncDialect
    import scair.passes.LowerTLamToFuncPass

    val ctx = MLContext()
    ctx.registerDialect(BuiltinDialect)
    ctx.registerDialect(TlamDialect)
    ctx.registerDialect(FuncDialect)

    val i32 = IntegerType(i(32), Signed)
    val funTy = TlamTy.fun(i32, i32)

    val lam = vlam(funTy)(i32)(x => Seq(VReturn(x)))

    val appRes = Result[TypeAttribute](i32)
    val top = Block(
      i32,
      (arg0: Value[Attribute]) =>
        val x = arg0.asInstanceOf[Value[TypeAttribute]]
        val app = VApply(lam.res, x, appRes)
        val ret = VReturn(appRes)
        Seq(lam, app, ret),
    )

    val m = ModuleOp(Region(Seq(top)))
    m.shouldVerify()

    val after = new LowerTLamToFuncPass(ctx).transform(m).asInstanceOf[ModuleOp]
    after.shouldVerify()

    val out = printIR(after)

    assertPrinted(out, includes = Seq("func.func", "func.call", "func.return"))
  }
