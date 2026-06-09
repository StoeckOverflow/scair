package scair.passes.dependent_product_tiling

import scair.ir.Operation
import scair.passes.analysis.ShapeProductFacts.FactorSelectionPolicy
import scair.passes.tiling.ValueDependentTiling

private[dependent_product_tiling] enum TailPolicy:
  case Exact
  case Guarded
  case Separable

private[dependent_product_tiling] enum ProductLoopKind:
  case ReductionOnly
  case AnyProductLoop

private[dependent_product_tiling] object DependentProductTilingTransform:
  private def mode(policy: TailPolicy): ValueDependentTiling.TailMode =
    policy match
      case TailPolicy.Exact     => ValueDependentTiling.TailMode.Exact
      case TailPolicy.Guarded   => ValueDependentTiling.TailMode.Guarded
      case TailPolicy.Separable => ValueDependentTiling.TailMode.Separable

  private def kind(loopKind: ProductLoopKind): ValueDependentTiling.ProductLoopKind =
    loopKind match
      case ProductLoopKind.ReductionOnly => ValueDependentTiling.ProductLoopKind.ReductionOnly
      case ProductLoopKind.AnyProductLoop => ValueDependentTiling.ProductLoopKind.AnyProductLoop

  def transform(
      op: Operation,
      tailPolicy: TailPolicy,
      factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
      loopKind: ProductLoopKind = ProductLoopKind.ReductionOnly,
  ): Operation =
    ValueDependentTiling.transformDAffineNatmulProduct(
      op,
      mode(tailPolicy),
      factorPolicy,
      kind(loopKind),
    )

  def transformOrdinaryIndexProduct(
      op: Operation,
      loopKind: ProductLoopKind = ProductLoopKind.ReductionOnly,
  ): Operation =
    ValueDependentTiling.transformDAffineOrdinaryProduct(op, kind(loopKind))

  def transformSeparableWhenNotExact(
      op: Operation,
      factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
      loopKind: ProductLoopKind = ProductLoopKind.ReductionOnly,
  ): Operation =
    ValueDependentTiling.transformDAffineProductByPolicy(
      op,
      ValueDependentTiling.TilingPolicy.SeparableWhenNotExact,
      factorPolicy,
      kind(loopKind),
    )
