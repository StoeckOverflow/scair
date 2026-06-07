package scair.ir

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.transformations.RewriteMethods

import scala.collection.mutable

final class BlockArgumentTypeUsesTest extends AnyFlatSpec:

  "Block argument type uses" should
    "register value references in argument types" in {
      val n = Value(I32)
      val buf = Value(ValueRefType(ValueAttribute(n)))
      val block = Block.fromArguments(Seq(n, buf), Seq.empty)

      n.typeUses.exists(tu =>
        (tu.owner eq block) && (tu.attribute.getVal() eq n)
      ) shouldBe true
    }

  it should "keep values live when used only by a block argument type" in {
    val n = Value(I32)
    val buf = Value(ValueRefType(ValueAttribute(n)))
    Block.fromArguments(Seq(n, buf), Seq.empty)

    assertThrows[Exception](n.erase())
  }

  it should "move block argument type uses during value replacement" in {
    val oldDim = Value(I32)
    val newDim = Value(I32)
    val buf = Value(ValueRefType(ValueAttribute(oldDim)))
    val block = Block.fromArguments(Seq(oldDim, newDim, buf), Seq.empty)

    RewriteMethods.replaceValue(oldDim, newDim)

    buf.typ.value shouldBe newDim
    oldDim.typeUses shouldBe empty
    newDim.typeUses.exists(tu =>
      (tu.owner eq block) && (tu.attribute.getVal() eq newDim)
    ) shouldBe true
  }

  it should "remap dependent argument types during block deep copy" in {
    val n = Value(I32)
    val buf = Value(ValueRefType(ValueAttribute(n)))
    val block = Block.fromArguments(Seq(n, buf), Seq.empty)

    val copied = block.deepCopy
    val copiedN = copied.arguments.head
    val copiedBuf = copied.arguments(1)

    copied should not be block
    copiedN should not be n
    copiedBuf should not be buf
    copiedBuf.typ.asInstanceOf[ValueRefType].value shouldBe copiedN
    copiedN.typeUses.exists(tu =>
      (tu.owner eq copied) && (tu.attribute.getVal() eq copiedN)
    ) shouldBe true
    n.typeUses.exists(tu => tu.owner eq copied) shouldBe false
  }

  it should
    "clone and remap fresh block argument types without mutating source types" in {
      val oldDim = Value(I32)
      val newDim = Value(I32)
      val sourceType = ValueRefType(ValueAttribute(oldDim))
      given mutable.Map[Value[Attribute], Value[Attribute]] =
        mutable.Map(oldDim -> newDim)

      val block = Block.cloneAndRemapArgumentTypes(Seq(sourceType), Seq.empty)
      val argType = block.arguments.head.typ.asInstanceOf[ValueRefType]

      sourceType.value shouldBe oldDim
      argType.value shouldBe newDim
      newDim.typeUses.exists(tu =>
        (tu.owner eq block) && (tu.attribute.getVal() eq newDim)
      ) shouldBe true
      oldDim.typeUses.exists(tu => tu.owner eq block) shouldBe false
    }
