package scair

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.tensor.*
import scair.ir.*

final class TensorTypesSpec extends AnyFlatSpec:

  "TensorTypeUtil.asTensor" should "convert vector to rank-1 tensor" in {
    val d0 = Value[Attribute](TensorNatType())
    val v = TensorVectorType(ValueAttribute(d0), Float32Type())
    val t = TensorTypeUtil.asTensor(v)
    t shouldBe TensorTensorType(Seq(ValueAttribute(d0)), Float32Type())
  }

  it should "convert matrix to rank-2 tensor" in {
    val d0 = Value[Attribute](TensorNatType())
    val d1 = Value[Attribute](TensorNatType())
    val m = TensorMatrixType(
      ValueAttribute(d0),
      ValueAttribute(d1),
      Float32Type(),
    )
    val t = TensorTypeUtil.asTensor(m)
    t shouldBe TensorTensorType(
      Seq(ValueAttribute(d0), ValueAttribute(d1)),
      Float32Type(),
    )
  }

  it should "return tensor unchanged" in {
    val n = Value[Attribute](TensorNatType())
    val tt = TensorTensorType(Seq(ValueAttribute(n)), Float32Type())
    TensorTypeUtil.asTensor(tt) shouldBe tt
  }
