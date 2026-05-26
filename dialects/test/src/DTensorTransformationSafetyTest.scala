package scair

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.ir.*
import scair.transformations.RewriteMethods
import scair.utils.Err
import scair.verify.Verifier

final class DTensorTransformationSafetyTest extends AnyFlatSpec:

  private def tensorOf(dim: Value[Attribute]): dTensorTensorType =
    dTensorTensorType(Seq(ValueAttribute(dim)), Float32Type())

  private def embeddedDim(t: dTensorTensorType): Value[Attribute] =
    t.params.head.getVal()

  "dtensor type uses" should "track and rewrite embedded dimension references during RAUW" in {
    val keptDim = Result(dTensorNatType())
    val replacedDim = Result(dTensorNatType())
    val kept = NatParam(keptDim)
    val replaced = NatParam(replacedDim)
    val user =
      UnregisteredOperation("test.tensor")(results =
        Seq(Result(tensorOf(replacedDim)))
      )
    val block = Block(operations = Seq(kept, replaced, user))

    replacedDim.typeUses.size shouldBe 1
    keptDim.typeUses.size shouldBe 0

    RewriteMethods.replaceValue(replacedDim, keptDim)

    embeddedDim(user.results.head.typ.asInstanceOf[dTensorTensorType]) should be
      theSameInstanceAs(keptDim)
    replacedDim.typeUses shouldBe empty
    keptDim.typeUses.size shouldBe 1
    block.operations.toSeq should contain(user)
  }

  "Block.deepCopy" should "remap embedded dtensor dimensions to copied SSA values without mutating the original" in {
    val dim = Result(dTensorNatType())
    val producer = NatParam(dim)
    val tensor = Empty(Result(tensorOf(dim)))
    val original = Block(operations = Seq(producer, tensor))

    val copied = original.deepCopy
    val copiedProducer = copied.operations.head.asInstanceOf[NatParam]
    val copiedTensor = copied.operations.toSeq(1).asInstanceOf[Empty]

    copiedProducer.res should not be theSameInstanceAs(dim)
    embeddedDim(copiedTensor.res.typ) should be theSameInstanceAs copiedProducer.res
    embeddedDim(tensor.res.typ) should be theSameInstanceAs dim
    embeddedDim(tensor.res.typ) should not be theSameInstanceAs(copiedProducer.res)
  }

  "Verifier" should "reject out-of-scope dtensor dimensions embedded in result types" in {
    val laterDim = Result(dTensorNatType())
    val use =
      UnregisteredOperation("test.tensor")(results =
        Seq(Result(tensorOf(laterDim)))
      )
    val defn = NatParam(laterDim)
    val module = ModuleOp(Region(Seq(Block(operations = Seq(use, defn)))))

    Verifier.verify(module) match
      case Err(msg, _) => msg should include("ssa-dominance")
      case _           => fail("expected embedded dimension dominance failure")
  }
