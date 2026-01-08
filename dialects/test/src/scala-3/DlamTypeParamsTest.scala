package scair

import scair.ir.*
import scair.Printer
import scair.parse.*
import scair.MLContext

import scair.dialects.builtin.*
import scair.dialects.dlam.*
import scair.passes.TypeParameterVerifierPass

import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.*

class DlamTypeParamsRoundTripTests extends AnyFlatSpec:

  given indentLevel: Int = 0

  /** Parse a module, run the Type params verifier pass (which also resolves
    * symbolic Type refs), then re-print it.
    */
  private def roundTrip(text: String): String =
    val ctx = MLContext()
    ctx.registerDialect(BuiltinDialect)
    ctx.registerDialect(DlamDialect)

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
   * - re-print contains the right dlam ops and !dlam.tvar forms
   */
  "SSA-polymorphic identity (ΛT. λ(x:T). x)" should
    "round-trip through parser + dep-type resolution + verifier" in {

      val source =
        """builtin.module {
          |  // F : ΛT. λ(x:T). x
          |  %F = "dlam.tlambda"() ({
          |  ^bb0(%T: !dlam.type):
          |    %v = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>}> ({
          |    ^bb0(%x: !dlam.tvar<%T>):
          |      "dlam.vreturn"(%x) <{expected = !dlam.tvar<%T>}> : (!dlam.tvar<%T>) -> ()
          |    }) : () -> (!dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>)
          |    "dlam.treturn"(%v)
          |      <{expected = !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>}>
          |      : (!dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>) -> ()
          |  }) : () -> (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)
          |}
          |""".stripMargin

      val printed = roundTrip(source).trim

      println("=== SSA poly-id round-trip ===")
      println(printed)

      printed should include("builtin.module")
      printed should include("dlam.tlambda")
      printed should include("dlam.vlambda")
      printed should include("dlam.vreturn")
      printed should include("dlam.treturn")

      printed should include("!dlam.tvar<%1>")
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
          |  %F = "dlam.tlambda"() ({
          |  ^bb0(%T: !dlam.type):
          |    // G = ΛU. λ(x:U).x
          |    %G = "dlam.tlambda"() ({
          |    ^bb0(%U: !dlam.type):
          |      %v = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>}> ({
          |      ^bb0(%x: !dlam.tvar<%U>):
          |        "dlam.vreturn"(%x) <{expected = !dlam.tvar<%U>}> : (!dlam.tvar<%U>) -> ()
          |      }) : () -> (!dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>)
          |
          |      "dlam.treturn"(%v)
          |        <{expected = !dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>}>
          |        : (!dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>) -> ()
          |    }) : () -> (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)
          |
          |    // h = G T : T → T
          |    %h = "dlam.tapply"(%G)
          |      <{argType = !dlam.tvar<%T>}>
          |      : (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)
          |        -> (!dlam.fun<!dlam.bvar<1>, !dlam.bvar<1>>)
          |
          |    // return h : T → T
          |    "dlam.treturn"(%h)
          |      <{expected = !dlam.fun<!dlam.bvar<1>, !dlam.bvar<1>>}>
          |      : (!dlam.fun<!dlam.bvar<1>, !dlam.bvar<1>>) -> ()
          |  }) : () -> (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)
          |}
          |""".stripMargin

      val printed = roundTrip(source).trim

      println("=== Nested SSA poly-id round-trip ===")
      println(printed)

      printed should include("builtin.module")
      printed should include("dlam.tlambda")
      printed should include("dlam.tapply")
      printed should include("dlam.vlambda")
      printed should include("dlam.vreturn")
      printed should include("dlam.treturn")

      printed should include("!dlam.tvar<%1>")
      printed should include("!dlam.tvar<%3>")
      printed should not include ("%T")
      printed should not include ("%U")

    }
