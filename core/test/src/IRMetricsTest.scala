package scair

import fastparse.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.analysis.IRMetrics
import scair.dialects.builtin.ModuleOp
import scair.parse.*

class IRMetricsTest extends AnyFlatSpec:
  private def parseModule(input: String): ModuleOp =
    val ctx = MLContext()
    val parser = new Parser(ctx, allowUnregisteredDialect = true)
    parser.parse(input, parser = moduleP(using _, parser)) match
      case Parsed.Success(m: ModuleOp, _) => m
      case failure: Parsed.Failure        =>
        fail(parser.error(failure))

  "IRMetrics" should "count operations, functions, blocks, block arguments, and tracked ops structurally" in {
    val module = parseModule(
      """
        |"builtin.module"() ({
        |  "func.func"() ({
        |  ^bb0(%arg0: memref<4xf32>, %arg1: memref<4xf32>):
        |    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
        |    %0 = "memref.load"(%arg0, %c0) : (memref<4xf32>, index) -> f32
        |    "memref.store"(%0, %arg1, %c0) : (f32, memref<4xf32>, index) -> ()
        |    "func.return"() : () -> ()
        |  }) {function_type = (memref<4xf32>, memref<4xf32>) -> (), sym_name = "example"} : () -> ()
        |}) : () -> ()
        |""".stripMargin
        .trim
    )

    val metrics = IRMetrics.collect(module)
    metrics.totalOps shouldBe 7
    metrics.funcDefs shouldBe 1
    metrics.blocks shouldBe 3
    metrics.blockArgs shouldBe 2
    metrics.opCount("builtin.module") shouldBe 2
    metrics.opCount("func.func") shouldBe 1
    metrics.opCount("memref.load") shouldBe 1
    metrics.opCount("memref.store") shouldBe 1
  }
