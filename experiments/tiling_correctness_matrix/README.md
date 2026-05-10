# Tiling Correctness Matrix

This structural benchmark records the small IR cases that support the tiling
correctness claims. It complements the matmul runtime benchmark by making the
compiler assumptions easy to audit.

## Cases

- `ordinary_tail`: ordinary `arith.muli` product; affine min tail must remain.
- `non_divisible_ordinary`: ordinary static product with tile size 3 over
  product 10; affine min tail must remain.
- `dependent_exact_dynamic`: explicit `dtensor.nat.mul` with a `!dtensor.posnat`
  factor; dynamic exact tile with no tail.
- `dependent_static_affine`: static `nat.const` factor; exact tile and bridge to
  stock `affine.for`.
- `runtime_checked_dynamic`: `cf.assert` positivity refinement; exact tile,
  assertion lowering, and late proof erasure.
- `zero_negative`: explicit `nat.const 0`; exact tiling must not happen.
- `nested_commuted_product`: `(K1 * K0) * K2`; explicit nested/commuted product
  exact-tiles without tail.
- `nested_commuted_product_lazy`: same product shape, but without the eager
  product canonicalization pass; exact tiling must still work.
- `tail_product_factor_lazy`: `(K1 * K0) * K2` tiled by `(K1 * K0)`; tail
  simplification removes the clamp by explicit product-factor subset reasoning.

## Run

```bash
experiments/tiling_correctness_matrix/build_tiling_correctness_matrix.sh
```

Useful environment variables:

- `SCAIR_OPT`
- `OUT_DIR`

## Outputs

- `out/metrics.csv`
- `out/metrics.json`
- `out/summary.md`
- `out/route_manifest.md`
- `out/route_manifest.json`
- per-case `.input.mlir` and `.tiled.mlir` artifacts

## Interpretation

This is not a runtime benchmark. It is a compact property matrix for route
claims: product representation, positivity source, tail behavior, dynamic/static
step counts, proof-op counts, and proof-erasure/lowering evidence.
