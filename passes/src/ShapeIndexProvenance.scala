package scair.passes

import scair.analysis.DominanceInfo
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.{cf, d_affine, d_memref, d_tensor as DTensor}
import scair.ir.*

import scala.collection.mutable

object ShapeIndexProvenance:
  private def valueRefTarget(v: Value[Attribute]): Value[Attribute] =
    v.typ match
      case ValueRefType(ref) => valueRefTarget(ref.getVal())
      case _                 => v

  private def containsRef(attr: Attribute, ref: ValueAttribute): Boolean =
    AttributeWalker.valueAttributesOf(attr).exists(_ eq ref)

  private def shapeTypeContainsRef(attr: Attribute, ref: ValueAttribute): Boolean =
    attr match
      case DTensor.DTensorTensorType(_, _) |
          DTensor.DTensorVectorType(_, _) |
          DTensor.DTensorMatrixType(_, _, _) =>
        containsRef(attr, ref)
      case d_memref.DMemrefMemrefType(_, _, _, _) |
          d_memref.DMemrefVectorType(_, _) |
          d_memref.DMemrefMatrixType(_, _, _) =>
        containsRef(attr, ref)
      case _ => false

  private def typeUseIsShapeRoot(tu: TypeUse): Boolean =
    tu.owner match
      case op: Operation =>
        op.operands.exists(v => shapeTypeContainsRef(v.typ, tu.attribute)) ||
          op.results.exists(v => shapeTypeContainsRef(v.typ, tu.attribute))
      case block: Block =>
        block.arguments.exists(v => shapeTypeContainsRef(v.typ, tu.attribute))

  private def hasAssumeExtentUse(v: Value[Attribute]): Boolean =
    v.uses.exists(use =>
      use.operation match
        case DTensor.AssumeExtent(extent) => extent eq v
        case _                            => false
    )

  private def hasDAffineShapeUse(v: Value[Attribute]): Boolean =
    v.uses.exists(use =>
      use.operation match
        case op: d_affine.Apply =>
          op.dimOperands.exists(_ eq v) || op.symbolOperands.exists(_ eq v)
        case op: d_affine.Min =>
          op.dimOperands.exists(_ eq v) || op.symbolOperands.exists(_ eq v)
        case op: d_affine.For =>
          op.lowerBoundOperands.exists(_ eq v) ||
            op.upperBoundOperands.exists(_ eq v) ||
            op.stepOperands.exists(_ eq v)
        case _ => false
    )

  private def rootOperation(op: Operation): Option[Operation] =
    var cur: IRNode = op
    var rootOp: Operation = op
    while cur.parent.nonEmpty do
      cur = cur.parent.get
      cur match
        case nextOp: Operation => rootOp = nextOp
        case _                 =>
    Some(rootOp)

  private def positiveAssertedValue(cmp: arith.CmpI): Option[Value[Attribute]] =
    def isZero(v: Value[Attribute]): Boolean =
      exactConstAny(v).contains(0)

    cmp.predicate match
      case arith.CmpIPredicate.sgt =>
        Option.when(isZero(cmp.rhs))(cmp.lhs.asInstanceOf[Value[Attribute]])
      case arith.CmpIPredicate.slt =>
        Option.when(isZero(cmp.lhs))(cmp.rhs.asInstanceOf[Value[Attribute]])
      case _ => None

  private def isProofUse(use: Use): Boolean =
    use.operation match
      case _: arith.CmpI => true
      case _: cf.Assert  => true
      case _             => false

  private def hasDominatingPositiveAssert(v: Value[Attribute]): Boolean =
    val base = valueRefTarget(v)
    if base.typ != IndexType() then false
    else
      val assertions =
        base.uses.toSeq.flatMap {
          case Use(cmp: arith.CmpI, _) if positiveAssertedValue(cmp).contains(base) =>
            cmp.results.headOption.toSeq.flatMap(_.uses.toSeq).flatMap {
              case Use(assertOp: cf.Assert, _) => Some(assertOp)
              case _                           => None
            }
          case _ => Seq.empty
        }

      assertions.exists { assertOp =>
        rootOperation(assertOp).exists { root =>
          val dom = DominanceInfo(root)
          base.uses.forall(use =>
            isProofUse(use) || dom.opDominates(assertOp, use.operation)
          )
        }
      }

  def isShapeRoot(v: Value[Attribute]): Boolean =
    v.typeUses.exists(typeUseIsShapeRoot) || hasAssumeExtentUse(v) || hasDAffineShapeUse(v)

  def resolveIndex(v: Value[Attribute]): Option[Value[Attribute]] =
    val base = valueRefTarget(v)
    base.typ match
      case _: IndexType if isShapeRoot(base) => Some(base)
      case _                                 => None

  def sameIndex(lhs: Value[Attribute], rhs: Value[Attribute]): Boolean =
    (resolveIndex(lhs), resolveIndex(rhs)) match
      case (Some(l), Some(r)) => l eq r
      case _                  => false

  def equivalentIndexOrConst(lhs: Value[Attribute], rhs: Value[Attribute]): Boolean =
    (resolveIndex(lhs), resolveIndex(rhs)) match
      case (Some(l), Some(r)) if l eq r => true
      case _ =>
        (exactConst(lhs).orElse(exactConstInShapeExpr(lhs)),
         exactConst(rhs).orElse(exactConstInShapeExpr(rhs))) match
          case (Some(l), Some(r)) => l == r
          case _                  => false

  private def exactConstAny(v: Value[Attribute]): Option[BigInt] =
    val memo = mutable.Map.empty[Value[Attribute], Option[BigInt]]
    val inProgress = mutable.Set.empty[Value[Attribute]]

    def eval(x: Value[Attribute]): Option[BigInt] =
      val base = valueRefTarget(x)
      memo.getOrElseUpdate(
        base, {
          if inProgress.contains(base) then None
          else
            inProgress += base
            val out = base.owner match
              case Some(arith.Constant(IntegerAttr(IntData(c), _: IndexType), _)) => Some(c)
              case Some(arith.AddI(lhs, rhs, _ , _)) if base.typ == IndexType() =>
                for
                  l <- eval(lhs)
                  r <- eval(rhs)
                yield l + r
              case Some(arith.MulI(lhs, rhs, _ , _)) if base.typ == IndexType() =>
                for
                  l <- eval(lhs)
                  r <- eval(rhs)
                yield l * r
              case _ => None
            inProgress -= base
            out
        },
      )

    eval(v)

  def exactConst(v: Value[Attribute]): Option[BigInt] =
    if resolveIndex(v).isDefined then exactConstAny(v) else None

  def exactConstInShapeExpr(v: Value[Attribute]): Option[BigInt] =
    exactConstAny(v)

  def isPositive(v: Value[Attribute]): Boolean =
    exactConst(v).exists(_ > 0) || hasDominatingPositiveAssert(v)
