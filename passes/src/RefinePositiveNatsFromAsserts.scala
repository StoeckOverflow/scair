package scair.passes.refine_positive_nats_from_asserts

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.cf
import scair.dialects.dTensor
import scair.ir.*
import scair.passes.NatProvenance
import scair.transformations.{ModulePass, RewriteMethods}
import scair.utils.OK

private val refinedMarker = "scair.refine_positive_nats_from_asserts.done"

private def asNatLike(v: Value[Attribute]): Operand[dTensor.dTensorNatLikeType] =
  v.asInstanceOf[Operand[dTensor.dTensorNatLikeType]]

private def asI1(v: Value[Attribute]): Operand[IntegerType] =
  v.asInstanceOf[Operand[IntegerType]]

private def strictPositiveNatFromProof(proof: Value[Attribute]): Option[Value[Attribute]] =
  proof.owner match
    case Some(arith.CmpI(lhs, rhs, _, predicate)) =>
      val leftNat = dTensor.dTensorTypeUtil.resolveNatFromIndexValue(lhs) match
        case OK(nat) => Some(nat)
        case _       => None
      val rightNat = dTensor.dTensorTypeUtil.resolveNatFromIndexValue(rhs) match
        case OK(nat) => Some(nat)
        case _       => None
      val leftConst = NatProvenance.exactConst(lhs)
      val rightConst = NatProvenance.exactConst(rhs)

      predicate match
        case arith.CmpIPredicate.sgt | arith.CmpIPredicate.ugt
            if rightConst.contains(0) =>
          leftNat
        case arith.CmpIPredicate.slt | arith.CmpIPredicate.ult
            if leftConst.contains(0) =>
          rightNat
        case _ => None
    case _ => None

private def mapOperands(
    operands: Seq[Value[Attribute]],
    from: Value[Attribute],
    to: Value[Attribute],
): Seq[Value[Attribute]] =
  operands.map(v => if v.asInstanceOf[AnyRef] eq from.asInstanceOf[AnyRef] then to else v)

private def rewriteLaterUsesInOpTree(
    op: Operation,
    from: Value[Attribute],
    to: Value[Attribute],
): Operation =
  val mappedOperands = mapOperands(op.operands, from, to)
  val updated =
    if mappedOperands == op.operands then op
    else
      val newOp = op.updated(
        operands = mappedOperands,
        results = op.results,
      )
      RewriteMethods.replaceOp(op, newOp, Some(newOp.results))
      newOp

  updated.regions.foreach(_.blocks.foreach(rewriteLaterUsesInBlock(_, from, to)))
  updated

private def rewriteLaterUsesInBlock(
    block: Block,
    from: Value[Attribute],
    to: Value[Attribute],
): Unit =
  block.operations.toSeq.foreach(op => rewriteLaterUsesInOpTree(op, from, to))

private def refineAfterAssert(assertOp: cf.Assert): Boolean =
  if assertOp.attributes.contains(refinedMarker) then false
  else
    strictPositiveNatFromProof(assertOp.arg) match
      case Some(nat) if !NatProvenance.isPositive(nat) =>
        val refine = dTensor.NatRefinePositive(
          asNatLike(nat),
          asI1(assertOp.arg),
          Result(dTensor.dTensorPosNatType()),
        )
        assertOp.containerBlock.foreach { block =>
          block.insertOpAfter(assertOp, refine)
          assertOp.attributes.addOne(refinedMarker -> StringData("true"))
          refine.next.foreach { firstAfterRefine =>
            val suffix = Iterator.iterate(Option(firstAfterRefine))(_.flatMap(_.next))
              .takeWhile(_.nonEmpty)
              .flatten
              .toSeq
            suffix.foreach(op => rewriteLaterUsesInOpTree(op, nat, refine.res))
          }
        }
        true
      case _ => false

final class RefinePositiveNatsFromAsserts(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "refine-positive-nats-from-asserts"

  override def transform(op: Operation): Operation =
    var changed = true
    while changed do
      changed = false
      def visit(cur: Operation): Unit =
        cur match
          case assertOp: cf.Assert =>
            if refineAfterAssert(assertOp) then changed = true
          case _ => ()
        cur.regions.foreach(_.blocks.foreach(_.operations.toSeq.foreach(visit)))
      visit(op)
    op
