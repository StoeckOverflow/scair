package scair

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.ir.*
import scair.utils.OK

final class TensorTypesSpec extends AnyFlatSpec:

  "TensorTypeUtil.asTensor" should "convert vector to rank-1 tensor" in {
    val d0 = Value[Attribute](DTensorNatType())
    val v = DTensorVectorType(ValueAttribute(d0), Float32Type())
    val t = DTensorTypeUtil.asDTensor(v)
    t shouldBe DTensorTensorType(Seq(ValueAttribute(d0)), Float32Type())
  }

  it should "convert matrix to rank-2 tensor" in {
    val d0 = Value[Attribute](DTensorNatType())
    val d1 = Value[Attribute](DTensorNatType())
    val m = DTensorMatrixType(
      ValueAttribute(d0),
      ValueAttribute(d1),
      Float32Type(),
    )
    val t = DTensorTypeUtil.asDTensor(m)
    t shouldBe DTensorTensorType(
      Seq(ValueAttribute(d0), ValueAttribute(d1)),
      Float32Type(),
    )
  }

  it should "return tensor unchanged" in {
    val n = Value[Attribute](DTensorNatType())
    val tt = DTensorTensorType(Seq(ValueAttribute(n)), Float32Type())
    DTensorTypeUtil.asDTensor(tt) shouldBe tt
  }

  "tensor.dim" should
    "return the embedded dim SSA value for the selected axis" in {
      val d0 = Value[Attribute](DTensorNatType())
      val d1 = Value[Attribute](DTensorNatType())
      val tensor = Value[DTensorTensorType](
        DTensorTensorType(
          Seq(ValueAttribute(d0), ValueAttribute(d1)),
          Float32Type(),
        )
      )
      val dim =
        Dim(
          tensor,
          IntegerAttr(IntData(1), I32),
          Result(ValueRefType(ValueAttribute(d1))),
        )

      dim.verify()
      dim.selectedDimValue shouldBe OK(d1)
    }
