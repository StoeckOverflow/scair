package scair

import fastparse.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.ir.*
import scair.parse.*
import scair.utils.Err
import scair.utils.OK

import scair.utils.{Err, OK}
import org.scalatest.Assertion
import org.scalatest.Assertions.{fail, succeed}
import org.scalatest.matchers.should.Matchers.*

extension [T](ok: OK[T])

  def shouldBeOK(clue: String = ""): Assertion =
    ok match
      case e: Err =>
        fail(
          if clue.isEmpty then s"Expected OK, got Err(${e.msg})"
          else s"$clue: Err(${e.msg})"
        )
      case _ =>
        succeed

final class TensorOpsSpec extends AnyFlatSpec:

  "tensor type parser/printer" should
    "round-trip tensor/vector/matrix with SSA dims" in {
      val ctx = MLContext()
      ctx.registerDialect(BuiltinDialect)
      ctx.registerDialect(dTensorDialect)
      val parser = Parser(ctx, allowUnregisteredDialect = true)

      val text =
        """builtin.module {
        |  %d0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
        |  %d1 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
        |  %v = "test.v"() : () -> !dtensor.vector<%d0, f32>
        |  %m = "test.m"() : () -> !dtensor.matrix<%d0, %d1, f32>
        |  %t = "test.t"() : () -> !dtensor.tensor<[%d0, %d1], f32>
        |}
        |""".stripMargin
      val parsed = parse(text, moduleP(using _, parser)).get.value
      val printed = parsed.toString
      printed should include("!dtensor.vector<%0, f32>")
      printed should include("!dtensor.matrix<%0, %1, f32>")
      printed should include("!dtensor.tensor<[%0, %1], f32>")
    }

  "tensor.add" should
    "reject shape mismatch when dims are semantically equal but not SSA-identical" in {
      val c4a = NatConst(IntegerAttr(IntData(4), I32), Result(dTensorNatType()))
      val c4b = NatConst(IntegerAttr(IntData(4), I32), Result(dTensorNatType()))
      val s0 = NatAdd(c4a.res, c4b.res, Result(dTensorNatType()))
      val s1 = NatAdd(c4a.res, c4b.res, Result(dTensorNatType()))

      val lhs = Value[dTensorTensorType](
        dTensorTensorType(Seq(ValueAttribute(s0.res)), Float32Type())
      )
      val rhs = Value[dTensorTensorType](
        dTensorTensorType(Seq(ValueAttribute(s1.res)), Float32Type())
      )
      val add = Add(lhs, rhs, Result(lhs.typ))

      add.verify() match
        case Err(msg) => msg should include("pairwise SSA-identical dims")
        case _        => fail("expected tensor.add verification failure")
    }

  "tensor.matmul" should "enforce shared inner dim SSA identity" in {
    val m = NatConst(
      IntegerAttr(IntData(2), I32),
      Result(dTensorNatType()),
    )
    val k0 = NatConst(IntegerAttr(IntData(3), I32), Result(dTensorNatType()))
    val k1 = NatConst(IntegerAttr(IntData(3), I32), Result(dTensorNatType()))
    val n = NatConst(IntegerAttr(IntData(5), I32), Result(dTensorNatType()))

    val lhs = Value[dTensorTensorType](
      dTensorTensorType(
        Seq(ValueAttribute(m.res), ValueAttribute(k0.res)),
        Float32Type(),
      )
    )
    val rhs = Value[dTensorTensorType](
      dTensorTensorType(
        Seq(ValueAttribute(k1.res), ValueAttribute(n.res)),
        Float32Type(),
      )
    )
    val res = Result(
      dTensorTensorType(
        Seq(ValueAttribute(m.res), ValueAttribute(n.res)),
        Float32Type(),
      )
    )
    val mm = Matmul(lhs, rhs, res)

    mm.verify() match
      case Err(msg) => msg should include("SSA-identical inner dims")
      case _        => fail("expected tensor.matmul verification failure")
  }

  "tensor.dim" should
    "return the embedded dim SSA value for the selected axis" in {
      val d0 = Value[Attribute](dTensorNatType())
      val d1 = Value[Attribute](dTensorNatType())
      val tensor = Value[dTensorTensorType](
        dTensorTensorType(
          Seq(ValueAttribute(d0), ValueAttribute(d1)),
          Float32Type(),
        )
      )
      val dim =
        Dim(tensor, IntegerAttr(IntData(1), I32), Result(dTensorNatType()))

      dim.verify().shouldBeOK()
      dim.selectedDimValue shouldBe OK(d1)
    }
