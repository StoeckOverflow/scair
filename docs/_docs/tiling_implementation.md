---
title: "Tiling Implementation"
---

# Tiling Implementation

This document describes the current ScaIR tiling implementation. The public pass
names are intentionally stable, but most tiling routes now share one internal
engine in `passes/src/tiling/ValueDependentTiling.scala`.

The important model is:

```text
loop IR
  -> normalized LoopDomain
  -> TilingFactProvider evidence
  -> proof queries
  -> TilingPlan
  -> affine/d_affine loop emitter
```

Pattern matching still exists, but it is localized inside fact providers. The
tiling emitter itself is not route-specific.

## Source Map

- `passes/src/tiling/ValueDependentTiling.scala`
  - shared domain extraction, fact providers, planner, body cloning, and loop
    emission for `affine.for` and `d_affine.for` tiling.
- `passes/src/DependentNatmulTilingTransform.scala`
  - compatibility facade used by product-loop pass wrappers.
- `passes/src/DependentProductLoopExactTile.scala`
  - pass wrappers for exact and separable dependent product-loop tiling.
- `passes/src/DependentExactTile.scala`
  - pass wrapper for reduction-like dependent exact tiling.
- `passes/src/DependentTileWithTailControl.scala`
  - pass wrapper for reduction-like guarded dependent tiling.
- `passes/src/OrdinaryAffineProductTileWithTail.scala`
  - ordinary `affine.for` product-loop tiling with a static tile size.
- `passes/src/ContextBandTileWithTail.scala`
  - output/context loop tiling for ordinary and dependent loops.
- `passes/src/DependentTailMinSimplify.scala`
  - removes provably unnecessary tail clamps after guarded tiling.
- `passes/src/analysis/NatProductFacts.scala`
  - product/factor reasoning for `dtensor.nat.mul` provenance.
- `passes/src/analysis/TailBoundFacts.scala`
  - recognizes `min(tile + tileSize, fullBound)` and proves when it can be
    removed.
- `passes/src/NatProvenance.scala`
  - resolves nat provenance, constants, positivity, and simple affine
    projections.
- `passes/src/RefinePositiveNatsFromAsserts.scala`
  - turns dominating positive `cf.assert` facts into `!dtensor.posnat`
    refinements consumed by tiling facts.
- `passes/src/DAffineToAffineCompatible.scala`
  - bridges eligible `d_affine.for` and `d_affine.if` forms to stock affine.
- `passes/src/DependentNatmulLoopFactorization.scala`
  - adjacent structural factorization pass. It rewrites flat product loops into
    nested loops, but it is not the generic tiling emitter.

## Public Pass Names

The public pass names remain stable and should be treated as compatibility entry
points:

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
  - dependent context-band tiling with a selected nat-product factor and a
    guarded tail.
- `dependent-context-band-exact-tile[:policy]`
  - dependent context-band tiling with a selected nat-product factor and no tail
    clamp.
- `dependent-context-band-separable-tile[:policy]`
  - dependent context-band tiling with full/partial separation.
- `dependent-tile-with-tail-control[:policy]`
  - guarded reduction-like dependent product-loop tiling.
- `dependent-product-loop-exact-tile[:policy]`
  - exact dependent product-loop tiling for any product loop.
- `dependent-product-loop-separable-tile[:policy]`
  - dependent product-loop tiling that prefers exact proofs and otherwise emits
    a full-tile/partial-tile split when the 1D affine guard can be represented.
- `dependent-exact-tile[:policy]`
  - exact reduction-like dependent product-loop tiling.

Factor policies are parsed by the pass CLI:

- `rightmost-positive`, the default;
- `leftmost-positive`;
- `factor-index=N`, where `N` is zero-based.

Static tile-size passes reject non-positive tile sizes before constructing the
pass.

## Loop Domains And Targets

`ValueDependentTiling` first normalizes eligible loops into `LoopDomain`.

The current supported domain shape is deliberately conservative:

- one-region, one-block loop body;
- exactly one lower-bound operand and one upper-bound operand;
- identity lower and upper affine maps;
- lower bound proven exactly `0` by `NatProvenance`;
- source loop step `1`;
- `d_affine.for` may use a dynamic step only after tiling has been emitted, not
  as the original source step.

The target kind is explicit:

- `ContextBand`
  - zero-result output/context loops only;
  - used for matmul output loops and Conv2D output-space loops.
- `ProductReduction`
  - loops with iter args and yielded results.
- `ExplicitLoop`
  - any eligible product loop requested by a product-loop pass.
- `MultiDimBand`
  - reserved for future multi-dimensional band tiling.

This target model prevents context-band passes from accidentally tiling
reduction loops. Conv2D output-only routes rely on that distinction.

## Fact Providers

Tile selection and proof discovery are fact-provider responsibilities.

Current providers:

- `NatMulFactProvider`
  - consumes `dtensor.nat.mul` provenance through `NatProductFacts`;
  - chooses a positive factor according to the pass factor policy;
  - provides exact-divisibility proof (`ProofSource.NatMul`).
- `OrdinaryProductFactProvider`
  - recognizes an ordinary `arith.muli` upper bound;
  - uses a known-positive multiplicand as the tile size;
  - does not provide exact-divisibility proof, so this route is guarded or
    separable rather than exact.
- `OrdinaryAffineProductBoundProvider`
  - supports ordinary `affine.for` product tiling where the upper bound is an
    `arith.muli`.
- `StaticTileFactProvider`
  - supplies static tile sizes for static context-band routes.
- `RefinedAssertFactProvider`
  - exposes positivity learned from `cf.assert`-driven refinement through
    `NatProvenance.isPositive`.
- `AffineSetFactProvider`
  - says whether the v1 1D full-tile condition can be represented as an affine
    set.

The remaining IR patterns are intentionally contained here. For example,
`arith.muli` recognition is a proof-source implementation detail, not a
separate tiling route.

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

- Is the tile size positive?
- Is the domain extent exactly divisible by the tile size?
- Can the full-tile condition be represented as an affine guard?

The current exact proof source is nat-product factor containment. Assert-derived
facts currently contribute primarily through positivity refinement. Direct
arbitrary inequality scanning from raw `cf.assert` is not implemented yet.

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
  - choose `Exact` when exact divisibility is proven;
  - otherwise choose `Guarded`.
- `GuardedOnly`
  - always choose `Guarded`.
- `SeparableWhenNotExact`
  - choose `Exact` when exact proof exists;
  - otherwise choose `Separable` when the 1D full-tile affine guard is
    representable;
  - otherwise fall back to `Guarded`.

Exact dependent tiling remains the preferred/default result when proof is
available.

## Emitted Loop Shapes

### Exact

```mlir
d_affine.for %tile = 0 to %N step %T {
  %tile_end = arith.addi %tile, %T
  d_affine.for %i = %tile to %tile_end step 1 {
    body(%i)
  }
}
```

For static tile sizes, the inner upper bound may be represented with an affine
shifted map instead of materializing `%tile_end`.

### Guarded

```mlir
d_affine.for %tile = 0 to %N step %T {
  %tile_end = arith.addi %tile, %T
  %clamped = arith.minsi %tile_end, %N
  d_affine.for %i = %tile to %clamped step 1 {
    body(%i)
  }
}
```

This is safe even when the tile size does not divide the full bound.

### Separable

V1 separable tiling is 1D:

```mlir
d_affine.for %tile = 0 to %N step %T {
  d_affine.if (%tile, %N, %T) [tile + T <= N] {
    d_affine.for %i = %tile to %tile + %T step 1 {
      body(%i)
    }
  } else {
    %tile_end = arith.addi %tile, %T
    %clamped = arith.minsi %tile_end, %N
    d_affine.for %i = %tile to %clamped step 1 {
      body(%i)
    }
  }
}
```

The condition is represented by an affine set equivalent to:

```text
tileIv + tileSize <= upperBound
```

Eligible `d_affine.if` regions can later be bridged to `affine.if` by
`d-affine-to-affine-compatible`.

## Body Cloning

The shared emitter clones the original loop body into the new inner loop.

It remaps:

- the old induction variable to the new inner induction variable;
- old iter args to new iter args;
- nested `affine.for`, `d_affine.for`, `affine.if`, and `d_affine.if` regions;
- external values by preserving their original SSA values.

The original loop is replaced with any required prelude operations plus the new
outer loop. Original loop results are remapped to the outer loop results.

## Nat Product Facts

`NatProductFacts.flattenProduct(v)` asks `NatProvenance.resolveNat(v)` for the
nat witness behind an index value. It then flattens nested `dtensor.nat.mul`
operations.

For example:

```mlir
%ab = "dtensor.nat.mul"(%a, %b)
%abc = "dtensor.nat.mul"(%ab, %c)
```

is modeled as:

```text
[%a, %b, %c]
```

The selected factor is chosen by policy:

- rightmost positive factor;
- leftmost positive factor;
- specific factor index.

Selection fails if a factor is exactly zero or if the selected factor is not
known positive. This prevents dynamic zero-step loops.

## Assert And Refinement Facts

`RefinePositiveNatsFromAsserts` recognizes assertions such as:

```mlir
%ok = arith.cmpi sgt %k_idx, %c0 : index
cf.assert %ok
```

and inserts:

```mlir
%k_pos = "dtensor.nat.refine_positive"(%k, %ok)
```

Later uses are rewritten to the positive nat where safe. `NatProvenance` then
sees `!dtensor.posnat`, and tiling providers can treat the corresponding tile
size as positive.

This is the current assert-derived path. The tiler does not yet scan arbitrary
raw dominating `cf.assert` inequalities directly.

## Tail-Min Simplification

`DependentTailMinSimplify.scala` handles clamps emitted by guarded routes. It
recognizes:

```text
min(tileIv + tileSize, fullBound)
```

where `tileIv + tileSize` may come from:

- `arith.addi`;
- `d_affine.apply`;
- `affine.apply`.

`TailBoundFacts.canDropClamp` requires:

- the clamp is inside the matching outer loop;
- the loop IV is the same SSA value as `tileIv`;
- the loop upper bound is the same SSA value as `fullBound`;
- the loop step is compatible with `tileSize`;
- `fullBound` contains `tileSize` as an explicit nat-product factor.

If the proof succeeds, the clamp result is replaced with the unclamped tile end.

## Ordinary Affine Tiling

Ordinary affine product tiling is the fixed-size baseline for `affine.for`.

It recognizes product bounds produced by ordinary `arith.muli`. Since ordinary
index arithmetic is not a dependent nat witness, the route does not claim exact
factor proof. It emits guarded affine tails.

The two product wrappers differ only in target selection:

- `ordinary-affine-product-tile-with-tail:N` requires reduction-like state;
- `ordinary-affine-product-loop-tile-with-tail:N` accepts any eligible product
  loop.

Context-band affine tiling similarly emits guarded static tiling for zero-result
output/context loops.

## Benchmark Interpretation

Benchmark metadata distinguishes:

- exact proof removed the tail entirely;
- guarded tiling retained tail protection;
- separable tiling split full and partial tiles with an affine guard.

For Matmul and Conv2D:

- output/context routes should tile only zero-result output loops;
- reduction/product routes should tile product/reduction loops only when those
  passes are explicitly run;
- full-factorized routes intentionally compose both kinds of pass.

Reduction tiling is currently structural evidence unless the benchmark route
also implements any required accumulation/privatization strategy.

## Current Boundaries

The implementation is a representative affine-style subset, not a full
polyhedral tiler.

Current limits:

- source loops must be simple zero-based unit-step 1D loops;
- multi-dimensional band selection is not implemented;
- separable tiling is 1D;
- direct arbitrary inequality reasoning from raw `cf.assert` is not implemented;
- non-identity affine bounds, non-zero lower bounds, and general semi-affine
  expressions are intentionally rejected;
- product-loop factorization remains a separate structural pass.

The architecture is now designed so these can be added as new domain
normalizers, fact providers, or proof queries instead of new hardcoded tiling
routes.
