package scair

import scair.dialects.builtin.*
import scair.dialects.tlam.*
import scair.dialects.tlam.TlamTy.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.ir.*

/** Keep only direct DBI helper tests here. Verifier diagnostics and textual IR
  * behavior are covered by FileCheck.
  */
final class TlamTypeParamsTest extends AnyFlatSpec:
  private inline def i(n: Int): IntData = IntData(n)
  private inline def b(n: Int): TlamBVarType = bvar(i(n))
  private inline def b0: TlamBVarType = b(0)
  private inline def b1: TlamBVarType = b(1)
  private inline def b2: TlamBVarType = b(2)
  private inline def alphaToAlphaAt(idx: Int): TlamFunType = fun(b(idx), b(idx))
  private inline def forall1(body: TypeAttribute): TlamForAllType = forall(body)

  "DBI.instantiate" should "compute (forall a. a->a)[i32] == i32->i32" in {
    val poly = forall1(alphaToAlphaAt(0))
    DBI.instantiate(poly, I32) shouldEqual fun(I32, I32)
  }

  "DBI.shift" should
    "bump only indices >= cutoff, distribute over fun, and respect binders" in {
      import DBI.*

      shift(1, 0, b0) shouldEqual b1
      shift(1, 1, b0) shouldEqual b0
      shift(0, 0, b(42)) shouldEqual b(42)

      val ty = fun(b0, fun(b1, b0))
      shift(1, 0, ty) shouldEqual fun(b1, fun(b2, b1))

      val poly = forall1(fun(b0, b1))
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
