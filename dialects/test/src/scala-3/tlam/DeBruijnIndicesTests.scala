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
import scair.dialects.tlam.verify.DeBruijnIndicesCheck

final class DeBruijnIndicesCheckTest extends AnyFlatSpec:

  // ------------------------------ shared runner ------------------------------

  private lazy val ctx: MLContext =
    val c = MLContext()
    c.registerDialect(BuiltinDialect)
    c.registerDialect(TlamDialect)
    c

  private def runVerifier(m: ModuleOp): Unit =
    m.shouldVerify()
    val checks = Verifier.defaultChecks :+ DeBruijnIndicesCheck
    Verifier.verify(m, checks = checks) match
      case e: Err => throw new Exception(e.msg)
      case _      => ()

  // ------------------------------ Tests ------------------------------

  // Λ.  λ (x : #1). x
  // Inside the TLambda body we have depth = 1, so only #0 is valid.
  "DeBruijn verifier" should "reject bvar<1> under a single TLambda binder" in {
    val badFunTy = fun(b1, b1) // invalid at depth=1

    val tl = tlam(forall1(badFunTy)) { (T: Value[Attribute]) =>
      val a = tvar(T)
      val vLam = vlam(fun(a, a))(a)(x => Seq(VReturn(x)))
      Seq(vLam, TReturn(vLam.res))
    }

    val m = module(tl)
    intercept[Exception](runVerifier(m))
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

  "DeBruijn verifier" should
    "accept bvar<1> inside forall when checked under an outer TLambda binder" in {

      val polyDef = polyIdDef()
      polyDef.shouldVerify()

      val outerResForall: TlamForAllType =
        forall1(alphaToAlphaAt(0)) // ∀. (#0 -> #0)

      val tl =
        tlam(outerResForall) { (T: Value[Attribute]) =>
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
