---
title: "Tiling Implementation"
---

# Tiling Implementation

This document describes the current ScaIR tiling implementation. Most tiling
routes share the engine in `passes/src/tiling/ValueDependentTiling.scala`.

The important model is:

```text
loop IR
  -> normalized LoopDomain
  -> TilingFactProvider evidence
  -> proof queries
  -> TilingPlan
  -> affine/d_affine loop emitter
```

Pattern matching is localized inside fact providers. The tiling emitter itself
is not route-specific.

## Source Map

- `passes/src/tiling/ValueDependentTiling.scala`
  - shared domain extraction, fact providers, planner, body cloning, and loop
    emission for `affine.for` and `d_affine.for` tiling.
- `passes/src/DependentProductTilingTransform.scala`
  - facade used by product-loop pass wrappers.
- `passes/src/DependentProductLoopExactTile.scala`
  - pass wrappers for exact and separable dependent product-loop tiling.
- `passes/src/DependentProductLoopFactorization.scala`
  - structural factorization of flat product loops into nested loops.
- `passes/src/DependentExactTile.scala`
  - reduction-like dependent exact tiling.
- `passes/src/DependentTileWithTailControl.scala`
  - reduction-like guarded dependent tiling.
- `passes/src/OrdinaryAffineProductTileWithTail.scala`
  - ordinary `affine.for` product-loop tiling with a static tile size.
- `passes/src/ContextBandTileWithTail.scala`
  - output/context loop tiling for ordinary and dependent loops.
- `passes/src/DependentTailMinSimplify.scala`
  - removes provably unnecessary tail clamps after guarded tiling.
- `passes/src/ShapeIndexProvenance.scala`
  - discovers shape-rooted `index` provenance, constants, and opt-in extent
    assumptions.
- `passes/src/analysis/ShapeProductFacts.scala`
  - product/factor reasoning for shape-rooted `arith.muli : index`.
- `passes/src/analysis/ShapeDivisibility.scala`
  - exact-divisibility queries over shape product facts.
- `passes/src/analysis/TailBoundFacts.scala`
  - recognizes `min(tile + tileSize, fullBound)` and proves when it can be
    removed.
- `passes/src/DAffineToAffineCompatible.scala`
  - bridges eligible `d_affine.for` and `d_affine.if` forms to stock affine.

## Public Pass Names

- `ordinary-affine-product-tile-with-tail:N`
  - guarded ordinary `affine.for` product-loop tiling;
  - requires reduction-like loop state.
- `ordinary-affine-product-loop-tile-with-tail:N`
  - guarded ordinary `affine.for` product-loop tiling for any product loop.
- `ordinary-affine-context-band-tile-with-tail:N`
  - guarded ordinary output/context-band tiling.
- `dependent-context-band-tile-with-tail:N`
  - guarded dependent output/context-band tiling with a static tile size.
- `dependent-context-band-factor-tile-with-tail[:policy]`
  - dependent context-band tiling with a selected shape-product factor and a
    guarded tail.
- `dependent-context-band-exact-tile[:policy]`
  - dependent context-band tiling with a selected shape-product factor and no
    tail clamp.
- `dependent-context-band-separable-tile[:policy]`
  - dependent context-band tiling with full/partial separation.
- `dependent-tile-with-tail-control[:policy]`
  - guarded reduction-like dependent product-loop tiling.
- `dependent-product-loop-exact-tile[:policy]`
  - exact dependent product-loop tiling for any product loop.
- `dependent-product-loop-separable-tile[:policy]`
  - dependent product-loop tiling that prefers exact proofs and otherwise emits
    a full-tile/partial-tile split when the 1D affine guard can be represented.
- `dependent-product-loop-factorization[:policy]`
  - structural factorization of product loops.
- `dependent-exact-tile[:policy]`
  - exact reduction-like dependent product-loop tiling.

Factor policies are parsed by the pass CLI:
- `rightmost-positive`, the default;
- `leftmost-positive`;
- `factor-index=N`, where `N` is zero-based.

Static tile-size passes reject non-positive tile sizes before constructing the
pass.

## Loop Domains And Targets

`ValueDependentTiling` first normalizes eligible loops into `LoopDomain`. The
current supported domain shape is deliberately conservative:
- one-region, one-block loop body;
- exactly one lower-bound operand and one upper-bound operand;
- identity lower and upper affine maps;
- lower bound proven exactly `0`;
- source loop step `1`;
- `d_affine.for` may use a dynamic step only after tiling has been emitted.

The target kind is explicit:
- `ContextBand` for zero-result output/context loops;
- `ProductReduction` for loops with iter args and yielded results;
- `ExplicitLoop` for any eligible product loop requested by a product-loop pass;
- `MultiDimBand`, reserved for future multi-dimensional band tiling.

This target model prevents context-band passes from accidentally tiling
reduction loops.

## Fact Providers

Tile selection and proof discovery are fact-provider responsibilities.

Current providers:
- `ShapeProductFactProvider`
  - consumes shape-rooted `arith.muli : index` provenance through
    `ShapeProductFacts`;
  - chooses a positive factor according to the pass factor policy;
  - provides exact-divisibility proof when a product factor is structurally
    contained in the shape product.
- `OrdinaryProductFactProvider`
  - recognizes an ordinary unrooted `arith.muli` upper bound;
  - uses a known-positive multiplicand as the tile size;
  - does not provide exact-divisibility proof, so this route is guarded or
    separable rather than exact.
- `OrdinaryAffineProductBoundProvider`
  - supports ordinary `affine.for` product tiling where the upper bound is an
    `arith.muli`.
- `StaticTileFactProvider`
  - supplies static tile sizes for static context-band routes.
- `AssumedExtentFactProvider`
  - exposes optional non-negativity or extent metadata only for passes that
    explicitly opt in; it does not prove strict positivity.
- `AffineSetFactProvider`
  - says whether the v1 1D full-tile condition can be represented as an affine
    set.

Arbitrary index arithmetic is not globally classified as shape provenance. A
shape-product proof starts from DTensor/DMemref/D-affine shape roots or from an
opt-in `d_tensor.assume_extent` marker that still exists at the time the
consumer runs.

## Proof Queries

The planner consumes `TilingProofs`:

```scala
final case class TilingProofs(
    positiveTileSize: Option[ProofSource],
    exactDivisibility: Option[ProofSource],
    fullTileFitsGuard: Option[ProofSource],
)
```

The core questions are:
- Is the tile size strictly positive?
- Is the domain extent exactly divisible by the tile size?
- Can the full-tile condition be represented as an affine guard?

Exact divisibility comes from structural shape-product containment. Positivity
requires a constant-positive fact or another explicit checked fact. Optional
`assume_extent` metadata is never strict positivity, and divisibility is not a
substitute for positivity.

## Planner Decisions

The planner produces one of:
- `Exact`
  - inner upper bound is `tileIv + tileSize`;
  - no tail clamp or partial branch.
- `Guarded`
  - inner upper bound is `min(tileIv + tileSize, fullUpperBound)`.
- `Separable`
  - emits a full-tile branch and a partial-tile branch.

Policies:
- `ExactPreferred`
  - choose `Exact` when exact divisibility and positive tile size are proven;
  - otherwise choose `Guarded`.
- `GuardedOnly`
  - always choose `Guarded`.
- `SeparableWhenNotExact`
  - choose `Exact` when exact proof exists;
  - otherwise choose `Separable` when the 1D full-tile affine guard is
    representable;
  - otherwise fall back to `Guarded`.

## Shape-Rooted Products

Shape products are ordinary MLIR arithmetic:

```mlir
%k = "arith.muli"(%k0, %k1) : (index, index) -> index
%buf = d_memref.reinterpret_cast %flat
  : !d_memref.memref<[], f32>
    to !d_memref.memref<[%k], f32>
```

Because `%k` is embedded in a shape, `ShapeProductFacts` can recover the nested
product structure. If the same `%k = arith.muli ...` appears only as unrelated
index arithmetic and is not connected to a shape root, dependent exact tiling
must not upgrade it to a proof-bearing product.

## Emitted Loop Shapes

Exact tiling emits a dynamic step and no tail clamp:

```mlir
d_affine.for %tile = 0 to %N step %T {
  %tile_end = "arith.addi"(%tile, %T) : (index, index) -> index
  d_affine.for %i = %tile to %tile_end step 1 {
    body(%i)
  }
}
```

Guarded tiling preserves the tail clamp:

```mlir
d_affine.for %tile = 0 to %N step %T {
  %tile_end = "arith.addi"(%tile, %T) : (index, index) -> index
  %clamped = "arith.minsi"(%tile_end, %N) : (index, index) -> index
  d_affine.for %i = %tile to %clamped step 1 {
    body(%i)
  }
}
```

`dependent-tail-min-simplify` may remove that clamp only when the final shape
facts prove it redundant.
