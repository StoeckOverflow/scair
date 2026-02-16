package scair

import fastparse.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.tensor.*
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
      ctx.registerDialect(TensorDialect)
      val parser = Parser(ctx, allowUnregisteredDialect = true)

      val text =
        """builtin.module {
        |  %d0 = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
        |  %d1 = "tensor.nat.const"() <{value = 7 : i32}> : () -> !tensor.nat
        |  %v = "test.v"() : () -> !tensor.vector<%d0, f32>
        |  %m = "test.m"() : () -> !tensor.matrix<%d0, %d1, f32>
        |  %t = "test.t"() : () -> !tensor.tensor<[%d0, %d1], f32>
        |}
        |""".stripMargin
      val parsed = parse(text, moduleP(using _, parser)).get.value
      val printed = parsed.toString
      printed should include("!tensor.vector<%0, f32>")
      printed should include("!tensor.matrix<%0, %1, f32>")
      printed should include("!tensor.tensor<[%0, %1], f32>")
    }

  "tensor.add" should
    "reject shape mismatch when dims are semantically equal but not SSA-identical" in {
      val c4a = NatConst(IntegerAttr(IntData(4), I32), Result(TensorNatType()))
      val c4b = NatConst(IntegerAttr(IntData(4), I32), Result(TensorNatType()))
      val s0 = NatAdd(c4a.res, c4b.res, Result(TensorNatType()))
      val s1 = NatAdd(c4a.res, c4b.res, Result(TensorNatType()))

      val lhs = Value[TensorTensorType](
        TensorTensorType(Seq(ValueAttribute(s0.res)), Float32Type())
      )
      val rhs = Value[TensorTensorType](
        TensorTensorType(Seq(ValueAttribute(s1.res)), Float32Type())
      )
      val add = Add(lhs, rhs, Result(lhs.typ))

      add.verify() match
        case Err(msg) => msg should include("pairwise SSA-identical dims")
        case _        => fail("expected tensor.add verification failure")
    }

  "tensor.matmul" should "enforce shared inner dim SSA identity" in {
    val m = NatConst(IntegerAttr(IntData(2), I32), Result(TensorNatType()))
    val k0 = NatConst(IntegerAttr(IntData(3), I32), Result(TensorNatType()))
    val k1 = NatConst(IntegerAttr(IntData(3), I32), Result(TensorNatType()))
    val n = NatConst(IntegerAttr(IntData(5), I32), Result(TensorNatType()))

    val lhs = Value[TensorTensorType](
      TensorTensorType(
        Seq(ValueAttribute(m.res), ValueAttribute(k0.res)),
        Float32Type(),
      )
    )
    val rhs = Value[TensorTensorType](
      TensorTensorType(
        Seq(ValueAttribute(k1.res), ValueAttribute(n.res)),
        Float32Type(),
      )
    )
    val res = Result(
      TensorTensorType(
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
      val d0 = Value[Attribute](TensorNatType())
      val d1 = Value[Attribute](TensorNatType())
      val tensor = Value[TensorTensorType](
        TensorTensorType(
          Seq(ValueAttribute(d0), ValueAttribute(d1)),
          Float32Type(),
        )
      )
      val dim =
        Dim(tensor, IntegerAttr(IntData(1), I32), Result(TensorNatType()))

      dim.verify().shouldBeOK()
      dim.selectedDimValue shouldBe OK(d1)
    }
