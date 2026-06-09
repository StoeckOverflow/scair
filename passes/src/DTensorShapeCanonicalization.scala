package scair.passes.d_tensor_shape_canonicalize

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.ir.*
import scair.passes.ShapeIndexProvenance
import scair.passes.analysis.ShapeProductFacts
import scair.transformations.{GreedyRewritePatternApplier, PatternRewriteWalker, WalkerPass, PatternAction, pattern}

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

private def idxConst(v: BigInt): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def dimAsIndex(param: DimParam): (Seq[Operation], Value[Attribute]) =
  param match
    case v: ValueAttribute => (Seq.empty, v.getVal())
    case IntegerAttr(IntData(v), _: IndexType | _: IntegerType) =>
      val c = idxConst(v)
      (Seq(c), c.result)

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
            val mul = arith.MulI(asIndex(acc), asIndex(dim), Result(IndexType()))
            (ops :+ mul, mul.result)
        }
      )

private def tensorTypeWithDims(
    dims: Seq[Value[Attribute]],
    elem: TypeAttribute,
): DTensorTensorType =
  DTensorTensorType(dims.map(ValueAttribute(_)), elem)

private def sameValueDims(
    lhs: Seq[Value[Attribute]],
    rhs: Seq[DimParam],
): Boolean =
  DTensorTypeUtil.sameDims(lhs.map(ValueAttribute(_)), rhs)

private def productMatches(
    product: Value[Attribute],
    factors: Seq[Value[Attribute]],
): Boolean =
  val explicit = ShapeProductFacts.ProductFactors(
    factors.map(f => ShapeProductFacts.Factor(f, ShapeIndexProvenance.exactConstInShapeExpr(f)))
  )
  ShapeProductFacts.flattenProduct(product).exists { full =>
    full.factors.size == explicit.factors.size &&
      full.containsAllExplicitFactors(explicit)
  }

private val MaterializeCollapseShapeProducts = pattern {
  case op: CollapseShape =>
    parseReassociationGroups(op.reassociation) match
      case None => PatternAction.Abort
      case Some(groups) =>
        val (resPrefix, resDims) = op.res.typ.params.map(dimAsIndex).unzip
        val (srcPrefix, srcDims) = op.src.typ.params.map(dimAsIndex).unzip
        val dimPrefix = (resPrefix ++ srcPrefix).flatten
        if groups.zipWithIndex.forall { case (group, resIdx) =>
            productMatches(
              resDims(resIdx),
              group.map(idx => srcDims(idx)),
            )
          }
        then PatternAction.Abort
        else
          val built = groups.map(group =>
            buildOrderedProduct(group.map(idx => srcDims(idx)))
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
              (dimPrefix ++ productOps :+ canonical, Seq(canonical.res))
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
        val (srcPrefix, srcDims) = op.src.typ.params.map(dimAsIndex).unzip
        val (resPrefix, resDims) = op.res.typ.params.map(dimAsIndex).unzip
        val dimPrefix = (srcPrefix ++ resPrefix).flatten
        if productMatches(
            resDims(idx),
            Seq(srcDims(idx), srcDims(idx + 1)),
          )
        then PatternAction.Abort
        else
          buildOrderedProduct(
            Seq(srcDims(idx), srcDims(idx + 1))
          ) match
            case None => PatternAction.Abort
            case Some((productOps, productDim)) =>
              val canonicalDims =
                srcDims.take(idx) ++
                  Seq(productDim) ++
                  srcDims.drop(idx + 2)
              if sameValueDims(canonicalDims, op.res.typ.params) then
                PatternAction.Abort
              else
                val canonical = JoinDim(
                  op.src,
                  op.dim,
                Result(tensorTypeWithDims(canonicalDims, op.res.typ.elem)),
              )
                (dimPrefix ++ productOps :+ canonical, Seq(canonical.res))
}

final class DTensorShapeCanonicalize(ctx: MLContext) extends WalkerPass(ctx):
  override val name = "tensor-shape-canonicalize"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(
      Seq(MaterializeCollapseShapeProducts, MaterializeJoinDimProduct)
    )
  )
