package scair

import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.ModuleOp
import scair.dialects.builtin.StringData
import scair.ir.ValueRefType
import scair.parse.Parser

class ParserValueRefResolutionTest extends AnyFlatSpec:

  private def parser = new Parser(MLContext(), allowUnregisteredDialect = true)

  "Parser value refs" should "preserve forward !value<%x> as unresolved placeholders" in {
    val input =
      """%u = "test.mk"() : () -> !value<%x>
        |%x = "test.make"() : () -> i32""".stripMargin

    parser.parse(input) match
      case Parsed.Success(m: ModuleOp, _) =>
        val ops = m.body.blocks.head.operations
        val useOp = ops.head
        val defOp = ops(1)
        val dep = useOp.results.head.typ.asInstanceOf[ValueRefType]

        dep.value.typ shouldEqual StringData("unresolved:%x")
        defOp.results.head.typeUses.size shouldEqual 0
      case other =>
        fail(s"expected successful parse, got: $other")
  }

  it should "reject unresolved !value references at scope end" in {
    val input =
      """%u = "test.mk"() : () -> !value<%x>""".stripMargin

    parser.parse(input) match
      case _: Parsed.Failure =>
        succeed
      case other =>
        fail(s"expected parse failure, got: $other")
  }

  it should "not resolve outer unresolved refs from inner region defs" in {
    val input =
      """%u = "test.mk"() : () -> !value<%x>
        |"test.with_region"() ({
        |  ^bb0:
        |    %x = "test.make"() : () -> i32
        |}) : () -> ()""".stripMargin

    parser.parse(input) match
      case _: Parsed.Failure =>
        succeed
      case other =>
        fail(s"expected parse failure, got: $other")
  }

  it should "preserve unresolved placeholders for multiple forward refs" in {
    val input =
      """%u = "test.mk1"() : () -> !value<%x>
        |%v = "test.mk2"() : () -> !value<%x>
        |%x = "test.make"() : () -> i32""".stripMargin

    parser.parse(input) match
      case Parsed.Success(m: ModuleOp, _) =>
        val ops = m.body.blocks.head.operations
        val depA = ops.head.results.head.typ.asInstanceOf[ValueRefType]
        val depB = ops(1).results.head.typ.asInstanceOf[ValueRefType]
        val x = ops(2).results.head

        depA.value.typ shouldEqual StringData("unresolved:%x")
        depB.value.typ shouldEqual StringData("unresolved:%x")
        x.typeUses.size shouldEqual 0
      case other =>
        fail(s"expected successful parse, got: $other")
  }
