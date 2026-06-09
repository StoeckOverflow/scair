package scair.passes.refine_positive_size_witnesses_from_asserts

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.cf
import scair.dialects.{d_tensor as DTensor}
import scair.ir.*
import scair.passes.SizeWitnessProvenance
import scair.transformations.{ModulePass, RewriteMethods}
import scair.utils.OK

private val refinedMarker = "scair.refine_positive_size_witnesses_from_asserts.done"

private def asSizeWitness(v: Value[Attribute]): Operand[DTensor.DTensorSizeWitnessType] =
  v.asInstanceOf[Operand[DTensor.DTensorSizeWitnessType]]

private def asI1(v: Value[Attribute]): Operand[IntegerType] =
  v.asInstanceOf[Operand[IntegerType]]

private def asPositiveProof(v: Value[Attribute]): Operand[DTensor.DTensorPositiveSizeProofType] =
  v.asInstanceOf[Operand[DTensor.DTensorPositiveSizeProofType]]

private def importedWitnessOf(index: Value[Attribute]): Option[Value[Attribute]] =
  index.uses.toSeq.collectFirst {
    case Use(op: DTensor.SizeImport, _) if op.index.asInstanceOf[AnyRef] eq index.asInstanceOf[AnyRef] =>
      op.res
  }

private def sizeWitnessForCmpOperand(v: Value[Attribute]): Option[Value[Attribute]] =
  DTensor.DTensorTypeUtil.resolveSizeWitnessFromIndexValue(v) match
    case OK(size) => Some(size)
    case _        => importedWitnessOf(v)

private def strictPositiveSizeWitnessFromProof(proof: Value[Attribute]): Option[Value[Attribute]] =
  proof.owner match
    case Some(arith.CmpI(lhs, rhs, _, predicate)) =>
      val leftWitness = sizeWitnessForCmpOperand(lhs)
      val rightWitness = sizeWitnessForCmpOperand(rhs)
      val leftConst = SizeWitnessProvenance.exactConst(lhs)
      val rightConst = SizeWitnessProvenance.exactConst(rhs)

      predicate match
        case arith.CmpIPredicate.sgt | arith.CmpIPredicate.ugt
            if rightConst.contains(0) =>
          leftWitness
        case arith.CmpIPredicate.slt | arith.CmpIPredicate.ult
            if leftConst.contains(0) =>
          rightWitness
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
    strictPositiveSizeWitnessFromProof(assertOp.arg) match
      case Some(witness) if !SizeWitnessProvenance.isPositive(witness) =>
        val proof = DTensor.SizePositiveProof(
          asSizeWitness(witness),
          asI1(assertOp.arg),
          Result(DTensor.DTensorPositiveSizeProofType()),
        )
        val refine = DTensor.SizeRefinePositive(
          asSizeWitness(witness),
          asPositiveProof(proof.res),
          Result(DTensor.DTensorPosSizeType()),
        )
        assertOp.containerBlock.foreach { block =>
          block.insertOpAfter(assertOp, proof)
          block.insertOpAfter(proof, refine)
          assertOp.attributes.addOne(refinedMarker -> StringData("true"))
          refine.next.foreach { firstAfterRefine =>
            val suffix = Iterator.iterate(Option(firstAfterRefine))(_.flatMap(_.next))
              .takeWhile(_.nonEmpty)
              .flatten
              .toSeq
            suffix.foreach(op => rewriteLaterUsesInOpTree(op, witness, refine.res))
          }
        }
        true
      case _ => false

final class RefinePositiveSizeWitnessesFromAsserts(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "refine-positive-size-witnesses-from-asserts"

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
