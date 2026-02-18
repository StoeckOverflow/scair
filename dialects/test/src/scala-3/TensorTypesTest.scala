package scair

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.ir.*

final class TensorTypesSpec extends AnyFlatSpec:

  "TensorTypeUtil.asTensor" should "convert vector to rank-1 tensor" in {
    val d0 = Value[Attribute](dTensorNatType())
    val v = dTensorVectorType(ValueAttribute(d0), Float32Type())
    val t = dTensorTypeUtil.asdTensor(v)
    t shouldBe dTensorTensorType(Seq(ValueAttribute(d0)), Float32Type())
  }

  it should "convert matrix to rank-2 tensor" in {
    val d0 = Value[Attribute](dTensorNatType())
    val d1 = Value[Attribute](dTensorNatType())
    val m = dTensorMatrixType(
      ValueAttribute(d0),
      ValueAttribute(d1),
      Float32Type(),
    )
    val t = dTensorTypeUtil.asdTensor(m)
    t shouldBe dTensorTensorType(
      Seq(ValueAttribute(d0), ValueAttribute(d1)),
      Float32Type(),
    )
  }

  it should "return tensor unchanged" in {
    val n = Value[Attribute](dTensorNatType())
    val tt = dTensorTensorType(Seq(ValueAttribute(n)), Float32Type())
    dTensorTypeUtil.asdTensor(tt) shouldBe tt
  }
