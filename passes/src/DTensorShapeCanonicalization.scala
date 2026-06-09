package scair.passes.d_tensor_shape_canonicalize

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.ir.*
import scair.utils.OK
import scair.transformations.{GreedyRewritePatternApplier, PatternRewriteWalker, WalkerPass, PatternAction, pattern}

private def constValue(
    v: Value[DTensorNatLikeType]
): Option[(BigInt, IntegerType | IndexType)] =
  v.owner match
    case Some(NatConst(IntegerAttr(IntData(k), typ), _)) => Some((k, typ))
    case _                                               => None

private def mkNatConst(
    k: BigInt,
    typ: IntegerType | IndexType,
    resType: DTensorNatLikeType,
): NatConst =
  NatConst(IntegerAttr(IntData(k), typ), Result(resType))

private def parseReassociationGroups(
    reassociation: ArrayAttribute[Attribute]
): Option[Seq[Seq[Int]]] =
  reassociation.attrValues.foldLeft[Option[Seq[Seq[Int]]]](Some(Seq.empty)) {
    case (acc, group: ArrayAttribute[?]) =>
      acc.flatMap(groups =>
        group.attrValues.foldLeft[Option[Seq[Int]]](Some(Seq.empty)) {
          case (groupAcc, IntegerAttr(IntData(idx), I32)) =>
            groupAcc.map(_ :+ idx.toInt)
          case _ => None
        }.map(indices => groups :+ indices)
      )
    case _ => None
  }

private def productType(
    lhs: Value[Attribute],
    rhs: Value[Attribute],
): DTensorNatLikeType =
  (lhs.typ, rhs.typ) match
    case (_: DTensorPosNatType, _: DTensorPosNatType) => DTensorPosNatType()
    case _                                            => DTensorNatType()

private def buildOrderedProduct(
    dims: Seq[Value[Attribute]]
): Option[(Seq[Operation], Value[Attribute])] =
  dims match
    case Seq() => None
    case Seq(dim) => Some((Seq.empty, dim))
    case first +: rest =>
      Some(
        rest.foldLeft((Seq.empty[Operation], first)) {
          case ((ops, acc), dim) =>
            val mul = NatMul(
              acc.asInstanceOf[Operand[DTensorNatLikeType]],
              dim.asInstanceOf[Operand[DTensorNatLikeType]],
              Result(productType(acc, dim)),
            )
            (ops :+ mul, mul.res)
        }
      )

private def tensorTypeWithDims(
    dims: Seq[Value[Attribute]],
    elem: TypeAttribute,
): DTensorTensorType =
  DTensorTensorType(dims.map(ValueAttribute(_)), elem)

private def sameValueDims(
    lhs: Seq[Value[Attribute]],
    rhs: Seq[ValueAttribute],
): Boolean =
  DTensorTypeUtil.sameDims(lhs.map(ValueAttribute(_)), rhs)

private def productMatches(
    product: Value[Attribute],
    factors: Seq[Value[Attribute]],
): Boolean =
  DTensorTypeUtil.sameOrderedNatProduct(product, factors) match
    case OK(true) => true
    case _        => false

private val NatAddFold = pattern { case NatAdd(lhs, rhs, res) =>
  (constValue(lhs), constValue(rhs)) match
    case (Some((0, _)), _)              => (Seq(), Seq(rhs))
    case (_, Some((0, _)))              => (Seq(), Seq(lhs))
    case (Some((a, aty)), Some((b, _))) =>
      mkNatConst(a + b, aty, res.typ)
    case _ => PatternAction.Abort
}

private val NatMulFold = pattern { case NatMul(lhs, rhs, res) =>
  (constValue(lhs), constValue(rhs)) match
    case (Some((0, _)), _)              => (Seq(), Seq(lhs))
    case (_, Some((0, _)))              => (Seq(), Seq(rhs))
    case (Some((1, _)), _)              => (Seq(), Seq(rhs))
    case (_, Some((1, _)))              => (Seq(), Seq(lhs))
    case (Some((a, aty)), Some((b, _))) =>
      mkNatConst(a * b, aty, res.typ)
    case _ => PatternAction.Abort
}

private val MaterializeCollapseShapeProducts = pattern {
  case op: CollapseShape =>
    parseReassociationGroups(op.reassociation) match
      case None => PatternAction.Abort
      case Some(groups) =>
        if groups.zipWithIndex.forall { case (group, resIdx) =>
            productMatches(
              op.res.typ.params(resIdx).getVal(),
              group.map(idx => op.src.typ.params(idx).getVal()),
            )
          }
        then PatternAction.Abort
        else
          val built = groups.map(group =>
            buildOrderedProduct(group.map(idx => op.src.typ.params(idx).getVal()))
          )
          if built.exists(_.isEmpty) then PatternAction.Abort
          else
            val products = built.flatten
            val productOps = products.flatMap(_._1)
            val canonicalDims = products.map(_._2)
            if sameValueDims(canonicalDims, op.res.typ.params) then PatternAction.Abort
            else
              val canonical = CollapseShape(
                op.src,
                op.reassociation,
                Result(tensorTypeWithDims(canonicalDims, op.res.typ.elem)),
              )
              (productOps :+ canonical, Seq(canonical.res))
}

private val MaterializeJoinDimProduct = pattern {
  case op: JoinDim =>
    if op.dim.typ != I32 then PatternAction.Abort
    else
      val axis = op.dim.value.value
      val srcRank = op.src.typ.params.size
      if axis < 0 || axis + 1 >= srcRank then PatternAction.Abort
      else
        val idx = axis.toInt
        if productMatches(
            op.res.typ.params(idx).getVal(),
            Seq(op.src.typ.params(idx).getVal(), op.src.typ.params(idx + 1).getVal()),
          )
        then PatternAction.Abort
        else
          buildOrderedProduct(
            Seq(op.src.typ.params(idx).getVal(), op.src.typ.params(idx + 1).getVal())
          ) match
            case None => PatternAction.Abort
            case Some((productOps, productDim)) =>
              val canonicalDims =
                op.src.typ.params.take(idx).map(_.getVal()) ++
                  Seq(productDim) ++
                  op.src.typ.params.drop(idx + 2).map(_.getVal())
              if sameValueDims(canonicalDims, op.res.typ.params) then
                PatternAction.Abort
              else
                val canonical = JoinDim(
                  op.src,
                  op.dim,
                  Result(tensorTypeWithDims(canonicalDims, op.res.typ.elem)),
                )
                (productOps :+ canonical, Seq(canonical.res))
}

final class DTensorShapeCanonicalize(ctx: MLContext) extends WalkerPass(ctx):
  override val name = "tensor-shape-canonicalize"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(
      Seq(NatAddFold, NatMulFold)
        ++ Seq(MaterializeCollapseShapeProducts, MaterializeJoinDimProduct)
    )
  )
