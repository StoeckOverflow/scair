package scair.passes.analysis

import scair.dialects.arith
import scair.dialects.builtin.*
import scair.ir.*
import scair.passes.ShapeIndexProvenance

object ShapeProductFacts:
  enum FactorSelectionPolicy:
    case RightmostPositive
    case LeftmostPositive
    case FactorIndex(index: Int)

    def label: String =
      this match
        case RightmostPositive => "rightmost-positive"
        case LeftmostPositive  => "leftmost-positive"
        case FactorIndex(idx)  => s"factor-index=$idx"

  enum FactorKey:
    case Const(value: BigInt)
    case Atom(value: Value[Attribute])

  final case class Factor(value: Value[Attribute], constValue: Option[BigInt]):
    def isPositive: Boolean =
      constValue.exists(_ > 0) || ShapeIndexProvenance.isPositive(value)

    def key: FactorKey =
      constValue match
        case Some(k) if value.owner.exists(_.isInstanceOf[arith.Constant]) =>
          FactorKey.Const(k)
        case _ => FactorKey.Atom(ShapeIndexProvenance.resolveIndex(value).getOrElse(value))

  final case class ProductFactors(factors: Seq[Factor]):
    private def sameExplicitFactor(lhs: Factor, rhs: Factor): Boolean =
      ShapeIndexProvenance.equivalentIndexOrConst(lhs.value, rhs.value) ||
        ((lhs.key, rhs.key) match
          case (FactorKey.Const(l), FactorKey.Const(r)) => l == r
          case (FactorKey.Atom(l), FactorKey.Atom(r))   => l eq r
          case _                                        => false
        )

    def containsEquivalentFactor(tileSize: Value[Attribute]): Boolean =
      val factor = Factor(tileSize, ShapeIndexProvenance.exactConstInShapeExpr(tileSize))
      factors.exists(sameExplicitFactor(_, factor))

    def removeOneEquivalentFactor(tileSize: Value[Attribute]): Option[Seq[Factor]] =
      val factor = Factor(tileSize, ShapeIndexProvenance.exactConstInShapeExpr(tileSize))
      val idx = factors.indexWhere(sameExplicitFactor(_, factor))
      if idx < 0 then None else Some(factors.patch(idx, Nil, 1))

    def containsAllExplicitFactors(factorProduct: ProductFactors): Boolean =
      removeAllExplicitFactors(factorProduct).isDefined

    def removeAllExplicitFactors(factorProduct: ProductFactors): Option[Seq[Factor]] =
      factorProduct.factors.foldLeft(Option(factors.toList)) {
        case (Some(remaining), factor) =>
          val idx = remaining.indexWhere(sameExplicitFactor(_, factor))
          if idx < 0 then None else Some(remaining.patch(idx, Nil, 1))
        case (None, _) => None
      }.map(_.toSeq)

    def rightmostPositiveFactor: Option[Factor] =
      if factors.exists(_.constValue.contains(BigInt(0))) then None
      else factors.reverse.find(_.isPositive)

    def leftmostPositiveFactor: Option[Factor] =
      if factors.exists(_.constValue.contains(BigInt(0))) then None
      else factors.find(_.isPositive)

    def selectFactor(policy: FactorSelectionPolicy): Option[Factor] =
      if factors.exists(_.constValue.contains(BigInt(0))) then None
      else
        policy match
          case FactorSelectionPolicy.RightmostPositive => factors.reverse.find(_.isPositive)
          case FactorSelectionPolicy.LeftmostPositive  => factors.find(_.isPositive)
          case FactorSelectionPolicy.FactorIndex(idx)  => factors.lift(idx).filter(_.isPositive)

    def keys: Seq[FactorKey] =
      factors.map(_.key)

    def sameFactorsModuloOrder(other: ProductFactors): Boolean =
      def removeOne(xs: List[FactorKey], key: FactorKey): Option[List[FactorKey]] =
        val idx = xs.indexWhere {
          case FactorKey.Const(v) => key == FactorKey.Const(v)
          case FactorKey.Atom(v) =>
            key match
              case FactorKey.Atom(w) => v eq w
              case _                 => false
        }
        if idx < 0 then None else Some(xs.patch(idx, Nil, 1))

      factors.map(_.key).foldLeft(Option(other.keys.toList)) {
        case (Some(remaining), key) => removeOne(remaining, key)
        case (None, _)             => None
      }.exists(_.isEmpty)

  def flattenProduct(v: Value[Attribute]): Option[ProductFactors] =
    ShapeIndexProvenance.resolveIndex(v).map(root => ProductFactors(flattenIndex(root)))

  def factorMultiset(v: Value[Attribute]): Option[ProductFactors] =
    flattenProduct(v)

  def containsFactor(fullBound: Value[Attribute], tileSize: Value[Attribute]): Boolean =
    flattenProduct(fullBound).exists(_.containsEquivalentFactor(tileSize))

  def containsExplicitFactor(product: Value[Attribute], factor: Value[Attribute]): Boolean =
    containsExplicitFactorProduct(product, factor)

  def containsExplicitFactorProduct(product: Value[Attribute], factor: Value[Attribute]): Boolean =
    val factorProduct =
      factorMultiset(factor).orElse(
        Some(ProductFactors(Seq(Factor(factor, ShapeIndexProvenance.exactConstInShapeExpr(factor)))))
      )
    (factorMultiset(product), factorProduct) match
      case (Some(full), Some(part)) => full.containsAllExplicitFactors(part)
      case _                       => false

  def sameProductModuloOrder(lhs: Value[Attribute], rhs: Value[Attribute]): Boolean =
    (factorMultiset(lhs), factorMultiset(rhs)) match
      case (Some(l), Some(r)) => l.sameFactorsModuloOrder(r)
      case _                  => false

  def residualAfterRemovingFactor(
      product: Value[Attribute],
      factor: Value[Attribute],
  ): Option[Seq[Factor]] =
    factorMultiset(product).flatMap(_.removeOneEquivalentFactor(factor))

  def residualAfterRemovingFactorProduct(
      product: Value[Attribute],
      factor: Value[Attribute],
  ): Option[Seq[Factor]] =
    (factorMultiset(product), factorMultiset(factor)) match
      case (Some(full), Some(part)) => full.removeAllExplicitFactors(part)
      case _                        => None

  def rightmostPositiveFactor(fullBound: Value[Attribute]): Option[Factor] =
    flattenProduct(fullBound).flatMap { product =>
      if product.factors.size < 2 then None else product.rightmostPositiveFactor
    }

  def selectFactor(
      fullBound: Value[Attribute],
      policy: FactorSelectionPolicy,
  ): Option[Factor] =
    flattenProduct(fullBound).flatMap { product =>
      if product.factors.size < 2 then None else product.selectFactor(policy)
    }

  def buildExplicitProduct(factors: Seq[Factor]): Option[(Seq[Operation], Value[Attribute])] =
    factors match
      case Seq() => None
      case Seq(factor) => Some((Seq.empty, factor.value))
      case first +: rest =>
        var prelude = Seq.empty[Operation]
        var acc = first.value
        rest.foreach { factor =>
          val mul = arith.MulI(
            acc.asInstanceOf[Operand[arith.AnyIntegerType]],
            factor.value.asInstanceOf[Operand[arith.AnyIntegerType]],
            Result(IndexType()),
          )
          prelude = prelude :+ mul
          acc = mul.result
        }
        Some((prelude, acc))

  private def flattenIndex(v: Value[Attribute]): Seq[Factor] =
    ShapeIndexProvenance.exactConstInShapeExpr(v) match
      case Some(k) if v.owner.exists(_.isInstanceOf[arith.Constant]) =>
        Seq(Factor(v, Some(k)))
      case _ =>
        v.owner match
          case Some(arith.MulI(lhs, rhs, _, _)) if v.typ == IndexType() =>
            flattenIndex(lhs) ++ flattenIndex(rhs)
          case _ =>
            Seq(Factor(v, ShapeIndexProvenance.exactConstInShapeExpr(v)))
