package scair.verify

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.Err

final class SSADominanceCheckTest extends AnyFlatSpec:

  final case class TestDepAttr(ref: ValueAttribute)
      extends TypeAttribute
      with ParametrizedAttribute:
    override def name: String = "test.dep"
    override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(ref)
    def v: Value[Attribute] = ref.getVal()

  "SSADominanceCheck" should "reject non-dominating dependent type uses" in {
    val laterDef = Result(I32)
    val defOp = UnregisteredOperation("test.def")(results = Seq(laterDef))

    val depAttr = TestDepAttr(ValueAttribute(laterDef))
    val useOp =
      UnregisteredOperation("test.use")(results = Seq(Result(depAttr)))

    val block = Block(operations = Seq(useOp, defOp))
    val module = ModuleOp(Region(Seq(block)))

    Verifier.verify(module) match
      case Err(msg) =>
        msg should include("ssa-dominance")
      case _ =>
        fail("expected dominance failure but verification succeeded")
  }
