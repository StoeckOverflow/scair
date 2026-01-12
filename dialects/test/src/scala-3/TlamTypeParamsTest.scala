package scair

import scair.ir.*
import scair.Printer
import scair.parse.*
import scair.MLContext

import scair.dialects.builtin.*
import scair.dialects.tlam.*
import scair.passes.TypeParameterVerifierPass

import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.*

class TlamTypeParamsRoundTripTests extends AnyFlatSpec:

  given indentLevel: Int = 0

  /** Parse a module, run the Type params verifier pass (which also resolves
    * symbolic Type refs), then re-print it.
    */
  private def roundTrip(text: String): String =
    val ctx = MLContext()
    ctx.registerDialect(BuiltinDialect)
    ctx.registerDialect(TlamDialect)

    val parser = new Parser(
      context = ctx,
      inputPath = None,
      parsingDiagnostics = true, // so parser.error returns a String
      allowUnregisteredDialect = false,
    )

    val parsed = parser.parse(
      text,
      (p: fastparse.P[?]) => moduleP(using p, parser),
      verboseFailures = true,
    )

    val module: ModuleOp =
      parsed.fold(
        (msg, idx, extra) => fail(s"Parse error:\n$msg\nat index $idx"),
        {
          case (m: ModuleOp, _) => m
          case (other, _)       =>
            fail(s"Expected ModuleOp at top level, got: $other")
        },
      )

    val pass = new TypeParameterVerifierPass(ctx)
    pass.transform(module)

    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    val printer = Printer(
      p = pw
    )

    printer.printTopLevel(module)
    pw.flush()
    sw.toString

  /*
   * ΛT. λ(x:T). x, encoded with SSA values in types
   *
   * Expected:
   * - parses
   * - TypeParameterVerifierPass succeed
   * - re-print contains the right tlam ops and !tlam.tvar forms
   */
  "SSA-polymorphic identity (ΛT. λ(x:T). x)" should
    "round-trip through parser + dep-type resolution + verifier" in {

      val source =
        """builtin.module {
          |  // F : ΛT. λ(x:T). x
          |  %F = "tlam.tlambda"() ({
          |  ^bb0(%T: !tlam.type):
          |    %v = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}> ({
          |    ^bb0(%x: !tlam.tvar<%T>):
          |      "tlam.vreturn"(%x) <{expected = !tlam.tvar<%T>}> : (!tlam.tvar<%T>) -> ()
          |    }) : () -> (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>)
          |    "tlam.treturn"(%v)
          |      <{expected = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}>
          |      : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
          |  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
          |}
          |""".stripMargin

      val printed = roundTrip(source).trim

      println("=== SSA poly-id round-trip ===")
      println(printed)

      printed should include("builtin.module")
      printed should include("tlam.tlambda")
      printed should include("tlam.vlambda")
      printed should include("tlam.vreturn")
      printed should include("tlam.treturn")

      printed should include("!tlam.tvar<%1>")
      printed should not include ("%T")
    }

  /*
   * ΛT.ΛU. λ(x:U). x, with a T-apply in between, also using SSA names in types.
   * This stresses that:
   *  - multiple SSA names (%T, %U) can appear in types
   *  - resolution finds the correct defining Values
   */
  "Nested SSA-polymorphic identity (ΛT.ΛU. λ(x:U). x)" should
    "round-trip and resolve both %T and %U" in {

      val source =
        """builtin.module {
          |  // F = ΛT.  (define G = ΛU. λ(x:U).x; h := G T; return h)
          |  %F = "tlam.tlambda"() ({
          |  ^bb0(%T: !tlam.type):
          |    // G = ΛU. λ(x:U).x
          |    %G = "tlam.tlambda"() ({
          |    ^bb0(%U: !tlam.type):
          |      %v = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>}> ({
          |      ^bb0(%x: !tlam.tvar<%U>):
          |        "tlam.vreturn"(%x) <{expected = !tlam.tvar<%U>}> : (!tlam.tvar<%U>) -> ()
          |      }) : () -> (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>)
          |
          |      "tlam.treturn"(%v)
          |        <{expected = !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>}>
          |        : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
          |    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
          |
          |    // h = G T : T → T
          |    %h = "tlam.tapply"(%G)
          |      <{argType = !tlam.tvar<%T>}>
          |      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
          |        -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>)
          |
          |    // return h : T → T
          |    "tlam.treturn"(%h)
          |      <{expected = !tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>}>
          |      : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>) -> ()
          |  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
          |}
          |""".stripMargin

      val printed = roundTrip(source).trim

      println("=== Nested SSA poly-id round-trip ===")
      println(printed)

      printed should include("builtin.module")
      printed should include("tlam.tlambda")
      printed should include("tlam.tapply")
      printed should include("tlam.vlambda")
      printed should include("tlam.vreturn")
      printed should include("tlam.treturn")

      printed should include("!tlam.tvar<%1>")
      printed should include("!tlam.tvar<%3>")
      printed should not include ("%T")
      printed should not include ("%U")

    }

  /*
   * Negative test: a nested type parameter (introduced as a block argument of an
   * inner `tlam.tlambda`) must not be usable outside that tlambda's region.
   */
  "Nested type parameter escape" should "be rejected by verify-type-params" in {

    val source =
      """builtin.module {
          |  %F = "tlam.tlambda"() ({
          |  ^bb0(%T: !tlam.type):
          |    // Inner type lambda introduces %U
          |    %G = "tlam.tlambda"() ({
          |    ^bb0(%U: !tlam.type):
          |      %v = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}> ({
          |      ^bb0(%x: !tlam.tvar<%T>):
          |        "tlam.vreturn"(%x) <{expected = !tlam.tvar<%T>}> : (!tlam.tvar<%T>) -> ()
          |      }) : () -> (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>)
          |      "tlam.treturn"(%v)
          |        <{expected = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}>
          |        : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
          |    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
          |
          |    // ILLEGAL: %U escapes its tlambda region here
          |    %w = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>}> ({
          |    ^bb0(%y: !tlam.tvar<%U>):
          |      "tlam.vreturn"(%y) <{expected = !tlam.tvar<%U>}> : (!tlam.tvar<%U>) -> ()
          |    }) : () -> (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>)
          |
          |    "tlam.treturn"(%G) <{expected = !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>}>
          |      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> ()
          |  }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>>)
          |}""".stripMargin

    val ex = intercept[Exception] {
      roundTrip(source)
    }

    ex.getMessage should include("not defined within Scope")
  }
