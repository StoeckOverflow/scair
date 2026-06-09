package scair

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.ir.*
import scair.transformations.RewriteMethods
import scair.utils.Err
import scair.verify.Verifier

final class DTensorTransformationSafetyTest extends AnyFlatSpec:

  private def tensorOf(dim: Value[Attribute]): DTensorTensorType =
    DTensorTensorType(Seq(ValueAttribute(dim)), Float32Type())

  private def tensorOf(dims: Seq[Value[Attribute]]): DTensorTensorType =
    DTensorTensorType(dims.map(ValueAttribute(_)), Float32Type())

  private def embeddedDim(t: DTensorTensorType): Value[Attribute] =
    t.params.head.asInstanceOf[ValueAttribute].getVal()

  private def embeddedDims(t: DTensorTensorType): Seq[Value[Attribute]] =
    t.params.map(_.asInstanceOf[ValueAttribute].getVal())

  private def reassociation(groups: Seq[Seq[Int]]): ArrayAttribute[Attribute] =
    ArrayAttribute(groups.map(group =>
      ArrayAttribute(group.map(idx => IntegerAttr(IntData(idx), I32)))
    ))

  private def i32Attr(value: Int): IntegerAttr =
    IntegerAttr(IntData(value), I32)

  private def i32Array(values: Seq[Int]): ArrayAttribute[Attribute] =
    ArrayAttribute(values.map(i32Attr))

  private def indexProducer(name: String, res: Result[Attribute]): Operation =
    UnregisteredOperation(name)(results = Seq(res))

  private def indexMulProducer(
      lhs: Value[Attribute],
      rhs: Value[Attribute],
      res: Result[Attribute],
  ): Operation =
    UnregisteredOperation("arith.muli")(operands = Seq(lhs, rhs), results = Seq(res))

  "d_tensor type uses" should "track and rewrite embedded dimension references during RAUW" in {
    val keptDim = Result(IndexType())
    val replacedDim = Result(IndexType())
    val kept = indexProducer("test.kept_dim", keptDim)
    val replaced = indexProducer("test.replaced_dim", replacedDim)
    val user =
      UnregisteredOperation("test.tensor")(results =
        Seq(Result(tensorOf(replacedDim)))
      )
    val block = Block(operations = Seq(kept, replaced, user))

    replacedDim.typeUses.size shouldBe 1
    keptDim.typeUses.size shouldBe 0

    RewriteMethods.replaceValue(replacedDim, keptDim)

    embeddedDim(user.results.head.typ.asInstanceOf[DTensorTensorType]) should be
      theSameInstanceAs(keptDim)
    replacedDim.typeUses shouldBe empty
    keptDim.typeUses.size shouldBe 1
    block.operations.toSeq should contain(user)
  }

  "Block.deepCopy" should "remap embedded d_tensor dimensions to copied SSA values without mutating the original" in {
    val dim = Result(IndexType())
    val producer = indexProducer("test.dim", dim)
    val tensor = Empty(Result(tensorOf(dim)))
    val original = Block(operations = Seq(producer, tensor))

    val copied = original.deepCopy
    val copiedProducer = copied.operations.head.asInstanceOf[UnregisteredOperation]
    val copieDTensor = copied.operations.toSeq(1).asInstanceOf[Empty]

    copiedProducer.results.head should not be theSameInstanceAs(dim)
    embeddedDim(copieDTensor.res.typ) should be theSameInstanceAs copiedProducer.results.head
    embeddedDim(tensor.res.typ) should be theSameInstanceAs dim
    embeddedDim(tensor.res.typ) should not be theSameInstanceAs(copiedProducer.results.head)
  }

  it should "remap collapse_shape embedded dimensions without mutating the original" in {
    val m = Result(IndexType())
    val n = Result(IndexType())
    val mn = Result(IndexType())
    val mProducer = indexProducer("test.m", m)
    val nProducer = indexProducer("test.n", n)
    val mnProducer = indexMulProducer(m, n, mn)
    val source = Empty(Result(tensorOf(Seq(m, n))))
    val collapse =
      CollapseShape(source.res, reassociation(Seq(Seq(0, 1))), Result(tensorOf(mn)))
    val original = Block(operations = Seq(mProducer, nProducer, mnProducer, source, collapse))

    val copied = original.deepCopy
    val copiedOps = copied.operations.toSeq
    val copiedM = copiedOps(0).asInstanceOf[UnregisteredOperation].results.head
    val copiedN = copiedOps(1).asInstanceOf[UnregisteredOperation].results.head
    val copiedMN = copiedOps(2).asInstanceOf[UnregisteredOperation].results.head
    val copiedSource = copiedOps(3).asInstanceOf[Empty]
    val copiedCollapse = copiedOps(4).asInstanceOf[CollapseShape]

    copiedM should not be theSameInstanceAs(m)
    copiedN should not be theSameInstanceAs(n)
    copiedMN should not be theSameInstanceAs(mn)
    embeddedDims(copiedSource.res.typ) should contain theSameElementsInOrderAs Seq(copiedM, copiedN)
    embeddedDim(copiedCollapse.res.typ) should be theSameInstanceAs copiedMN
    embeddedDims(source.res.typ) should contain theSameElementsInOrderAs Seq(m, n)
    embeddedDim(collapse.res.typ) should be theSameInstanceAs mn
  }

  it should "remap join_dim embedded dimensions without mutating the original" in {
    val m = Result(IndexType())
    val n = Result(IndexType())
    val mn = Result(IndexType())
    val mProducer = indexProducer("test.m", m)
    val nProducer = indexProducer("test.n", n)
    val mnProducer = indexMulProducer(m, n, mn)
    val joinSource = Empty(Result(tensorOf(Seq(m, n))))
    val join = JoinDim(joinSource.res, i32Attr(0), Result(tensorOf(mn)))
    val original = Block(operations = Seq(mProducer, nProducer, mnProducer, joinSource, join))

    val copied = original.deepCopy
    val copiedOps = copied.operations.toSeq
    val copiedM = copiedOps(0).asInstanceOf[UnregisteredOperation].results.head
    val copiedN = copiedOps(1).asInstanceOf[UnregisteredOperation].results.head
    val copiedMN = copiedOps(2).asInstanceOf[UnregisteredOperation].results.head
    val copiedJoinSource = copiedOps(3).asInstanceOf[Empty]
    val copiedJoin = copiedOps(4).asInstanceOf[JoinDim]

    copiedM should not be theSameInstanceAs(m)
    copiedN should not be theSameInstanceAs(n)
    copiedMN should not be theSameInstanceAs(mn)
    embeddedDims(copiedJoinSource.res.typ) should contain theSameElementsInOrderAs Seq(copiedM, copiedN)
    embeddedDim(copiedJoin.res.typ) should be theSameInstanceAs copiedMN
    embeddedDims(joinSource.res.typ) should contain theSameElementsInOrderAs Seq(m, n)
    embeddedDim(join.res.typ) should be theSameInstanceAs mn
  }

  it should "remap split_dim embedded dimensions without mutating the original" in {
    val m = Result(IndexType())
    val mt = Result(IndexType())
    val tm = Result(IndexType())
    val n = Result(IndexType())
    val mProducer = indexProducer("test.m", m)
    val mtProducer = indexProducer("test.mt", mt)
    val tmProducer = indexProducer("test.tm", tm)
    val nProducer = indexProducer("test.n", n)
    val splitSource = Empty(Result(tensorOf(Seq(m, n))))
    val split = SplitDim(splitSource.res, mt, tm, i32Attr(0), Result(tensorOf(Seq(mt, tm, n))))
    val original =
      Block(operations = Seq(mProducer, mtProducer, tmProducer, nProducer, splitSource, split))

    val copied = original.deepCopy
    val copiedOps = copied.operations.toSeq
    val copiedM = copiedOps(0).asInstanceOf[UnregisteredOperation].results.head
    val copiedMT = copiedOps(1).asInstanceOf[UnregisteredOperation].results.head
    val copiedTM = copiedOps(2).asInstanceOf[UnregisteredOperation].results.head
    val copiedN = copiedOps(3).asInstanceOf[UnregisteredOperation].results.head
    val copiedSplitSource = copiedOps(4).asInstanceOf[Empty]
    val copiedSplit = copiedOps(5).asInstanceOf[SplitDim]

    copiedM should not be theSameInstanceAs(m)
    copiedMT should not be theSameInstanceAs(mt)
    copiedTM should not be theSameInstanceAs(tm)
    copiedN should not be theSameInstanceAs(n)
    embeddedDims(copiedSplitSource.res.typ) should contain theSameElementsInOrderAs Seq(copiedM, copiedN)
    copiedSplit.outer should be theSameInstanceAs copiedMT
    copiedSplit.inner should be theSameInstanceAs copiedTM
    embeddedDims(copiedSplit.res.typ) should contain theSameElementsInOrderAs Seq(copiedMT, copiedTM, copiedN)
    embeddedDims(splitSource.res.typ) should contain theSameElementsInOrderAs Seq(m, n)
    split.outer should be theSameInstanceAs mt
    split.inner should be theSameInstanceAs tm
    embeddedDims(split.res.typ) should contain theSameElementsInOrderAs Seq(mt, tm, n)
  }

  it should "remap permute_dims embedded dimensions without mutating the original" in {
    val m = Result(IndexType())
    val n = Result(IndexType())
    val mProducer = indexProducer("test.m", m)
    val nProducer = indexProducer("test.n", n)
    val source = Empty(Result(tensorOf(Seq(m, n))))
    val permute = PermuteDims(source.res, i32Array(Seq(1, 0)), Result(tensorOf(Seq(n, m))))
    val original = Block(operations = Seq(mProducer, nProducer, source, permute))

    val copied = original.deepCopy
    val copiedOps = copied.operations.toSeq
    val copiedM = copiedOps(0).asInstanceOf[UnregisteredOperation].results.head
    val copiedN = copiedOps(1).asInstanceOf[UnregisteredOperation].results.head
    val copiedSource = copiedOps(2).asInstanceOf[Empty]
    val copiedPermute = copiedOps(3).asInstanceOf[PermuteDims]

    copiedM should not be theSameInstanceAs(m)
    copiedN should not be theSameInstanceAs(n)
    embeddedDims(copiedSource.res.typ) should contain theSameElementsInOrderAs Seq(copiedM, copiedN)
    embeddedDims(copiedPermute.res.typ) should contain theSameElementsInOrderAs Seq(copiedN, copiedM)
    embeddedDims(source.res.typ) should contain theSameElementsInOrderAs Seq(m, n)
    embeddedDims(permute.res.typ) should contain theSameElementsInOrderAs Seq(n, m)
  }

  "Verifier" should "reject out-of-scope d_tensor dimensions embedded in result types" in {
    val laterDim = Result(IndexType())
    val use =
      UnregisteredOperation("test.tensor")(results =
        Seq(Result(tensorOf(laterDim)))
      )
    val defn =
      UnregisteredOperation("test.dim")(results = Seq(laterDim))
    val module = ModuleOp(Region(Seq(Block(operations = Seq(use, defn)))))

    Verifier.verify(module) match
      case Err(msg, _) => msg should include("ssa-dominance")
      case _           => fail("expected embedded dimension dominance failure")
  }
