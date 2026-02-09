package scair

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.tensor.*
import scair.ir.*

final class TensorTypesSpec extends AnyFlatSpec:

  "TensorTypeUtil.asTensor" should "convert vector to rank-1 tensor" in {
    val v = TensorVectorType(IntegerAttr(IntData(4), I64), Float32Type())
    val t = TensorTypeUtil.asTensor(v)
    t shouldBe TensorTensorType(
      Seq(IntegerAttr(IntData(4), I64)),
      Float32Type(),
    )
  }

  it should "convert matrix to rank-2 tensor" in {
    val m = TensorMatrixType(
      IntegerAttr(IntData(2), I64),
      IntegerAttr(IntData(3), I64),
      Float32Type(),
    )
    val t = TensorTypeUtil.asTensor(m)
    t shouldBe TensorTensorType(
      Seq(IntegerAttr(IntData(2), I64), IntegerAttr(IntData(3), I64)),
      Float32Type(),
    )
  }

  it should "return tensor unchanged" in {
    val n = Value[Attribute](IndexType())
    val tt = TensorTensorType(Seq(ValueAttribute(n)), Float32Type())
    TensorTypeUtil.asTensor(tt) shouldBe tt
  }
