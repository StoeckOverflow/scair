package scair

import scair.MLContext
import scair.ir.*
import scair.dialects.testutils.IRTestKit.*
import scair.dialects.builtin.*
import scair.dialects.builtin.BuiltinDialect
import scair.verify.Verifier
import scair.utils.Err
import scair.dialects.tlam.TlamDialect
import scair.dialects.tlam.*
import scair.dialects.tlam.TlamTy.*
import scair.testutils.tlam.TlamTestIR.*
import org.scalatest.flatspec.AnyFlatSpec

final class DeBruijnIndicesCheckTest extends AnyFlatSpec:

  // ------------------------------ shared runner ------------------------------

  private lazy val ctx: MLContext =
    val c = MLContext()
    c.registerDialect(BuiltinDialect)
    c.registerDialect(TlamDialect)
    c

  private def runVerifier(m: ModuleOp): Unit =
    m.shouldVerify()
    Verifier.verify(m, ctx) match
      case e: Err => throw new Exception(e.msg)
      case _      => ()

  // ------------------------------ Tests ------------------------------

  // Λ.  λ (x : #1). x
  // Inside the TLambda body we have depth = 1, so only #0 is valid.
  "DeBruijn verifier" should "reject bvar<1> under a single TLambda binder" in {
    val badFunTy = fun(b1, b1) // invalid at depth=1

    // IMPORTANT: TLambda.verify compares against its result type (after normalization),
    // so we must set the TLambda result body to exactly what we return.
    val tl = tlam(forall1(badFunTy)) { (_: Value[Attribute]) =>
      val vLam = vlam(badFunTy)(b1)(x => Seq(VReturn(x)))
      Seq(vLam, TReturn(vLam.res))
    }

    val m = module(tl)
    intercept[Exception](runVerifier(m))
  }

  // Λ. Λ. λ (x : #1). x
  // Inside the inner TLambda body we have depth = 2:
  //   #0 = inner binder, #1 = outer binder.
  "DeBruijn verifier" should
    "accept bvar<1> when two TLambda binders are in scope" in {

      val funTy = fun(b1, b1) // valid at depth=2
      val innerForall = forall1(funTy)
      val outerForall = forall1(innerForall)

      val outerTL =
        tlam(outerForall) { (_: Value[Attribute]) =>
          val innerTL =
            tlam(innerForall) { (_: Value[Attribute]) =>
              val vLam = vlam(funTy)(b1)(x => Seq(VReturn(x)))
              Seq(vLam, TReturn(vLam.res))
            }
          Seq(innerTL, TReturn(innerTL.res))
        }

      val print_IR = printIR(outerTL)
      println(print_IR)
      val m = module(outerTL)
      runVerifier(m)
    }

  "DeBruijn verifier" should
    "accept bvar<1> when two TLambda binders are in scope working" in {

      // Inner TLambda returns a value of type: forall (b1 -> b1)
      val innerSchema: TlamForAllType = forall1(fun(b1, b1))

      // Outer TLambda returns a value of type: forall ( forall (b1 -> b1) )
      // i.e. outer schema body is exactly innerSchema
      val outerSchema: TlamForAllType = forall1(innerSchema)

      val outerTL =
        tlam(outerSchema) { (_: Value[Attribute]) =>
          val innerTL =
            tlam(innerSchema) { (_: Value[Attribute]) =>
              val vLam = vlam(fun(b1, b1))(b1)(x => Seq(VReturn(x)))
              Seq(vLam, TReturn(vLam.res))
            }
          Seq(innerTL, TReturn(innerTL.res))
        }

      val print_IR = printIR(outerTL)
      println(print_IR)

      val m = module(outerTL)
      runVerifier(m)
    }

  // (top-level contains)  (∀. (#0 -> #0)) [ ∀. (#1 -> #0) ]
  // At top-level there is no outer binder. Inside the tyArg's forall body the
  // depth is 1 => only #0 is valid, so #1 is illegal.
  "DeBruijn verifier" should
    "reject bvar<1> inside a forall body at top-level" in {

      // bad forall: inside its body depth=1 => only b0 allowed; b1 invalid
      val badPoly: TlamForAllType = forall1(fun(b1, b0))

      val polyDef = polyIdDef()
      polyDef.shouldVerify()

      val tapp = TApply(
        fun = polyDef.res,
        tyArg = badPoly,
        res = Result[TypeAttribute](fun(badPoly, badPoly)),
      )
      tapp.verify().shouldBeOK("verify failed for tapply")

      val m = module(polyDef, tapp)
      intercept[Exception](runVerifier(m))
    }

  // We still want to test that (#1 under an outer TLambda) becomes in-scope for a nested forall body.
  //
  // But: your TLambda.verify now normalizes its expected return type by substituting
  // bvar(0) with tvar(%T), and that substitution *also* turns references to the outer binder
  // inside nested forall bodies into tvar(%T) rather than bvar(1).
  //
  // So, to keep this test focused on the DeBruijn pass (not TLambda return-type normalization),
  // we place the "goodPoly = ∀. (#1 -> #0)" only as a TApply tyArg inside the TLambda body,
  // while returning something unrelated (and well-typed) from the TLambda.
  "DeBruijn verifier" should
    "accept bvar<1> inside forall when checked under an outer TLambda binder" in {

      val polyDef = polyIdDef()
      polyDef.shouldVerify()

      // Outer TLambda just returns the identity function for its binder (SSA tvar form),
      // but in its body we also include a TApply whose tyArg uses b1 inside a forall body.
      val outerResForall: TlamForAllType =
        forall1(alphaToAlphaAt(0)) // ∀. (#0 -> #0)

      val tl =
        tlam(outerResForall) { (T: Value[Attribute]) =>
          // This is the test payload:
          // Under outer TLambda: depth=1. Under this forall body: depth=2, so b1 is valid.
          val goodPolyDB: TlamForAllType = forall1(fun(b1, b0))

          val tapp = TApply(
            fun = polyDef.res,
            tyArg = goodPolyDB,
            res = Result[TypeAttribute](fun(goodPolyDB, goodPolyDB)),
          )
          tapp.verify().shouldBeOK("verify failed for tapply")

          // Now return a well-typed value matching TLambda.verify normalization:
          // expected = DBI.subst(0, tvar(T), fun(b0,b0)) == fun(tvar(T), tvar(T))
          val a = tvar(T)
          val idV = vlam(fun(a, a))(a)(x => Seq(VReturn(x)))
          idV.shouldVerify()

          Seq(polyDef, tapp, idV, TReturn(idV.res))
        }

      val m = module(tl)
      runVerifier(m)
    }

  // (∀. (#0 -> #0)) [ #0 ]
  // The application is at module top-level where depth = 0 (no binders in scope).
  // Any bvar<#k> is out of scope at depth 0, so #0 is illegal.
  "DeBruijn verifier" should
    "reject TApply tyArg bvar<0> at top-level (depth=0)" in {

      val polyDef = polyIdDef()
      polyDef.shouldVerify()

      val bad = TApply(
        fun = polyDef.res,
        tyArg = b0,
        res = Result[TypeAttribute](fun(b0, b0)),
      )
      bad.verify().shouldBeOK("verify failed for tapply")

      val m = module(polyDef, bad)
      intercept[Exception](runVerifier(m))
    }
