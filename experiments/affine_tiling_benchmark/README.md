# Affine Tiling Benchmark

This is a small structural benchmark for product-loop tiling. It is supporting
evidence for the matmul affine bridge, not a separate runtime benchmark.

## Variants

The script accepts canonical aliases from
[`../tiling_benchmark_routes.md`](../tiling_benchmark_routes.md), and emits the
accepted route map to `out/route_manifest.md` for each run.

- `mlir_runtime_product`: ordinary stock affine/MLIR product bound, tiled with a
  static tile size, expected to keep a min/tail bound.
- `mlir_static_factor_reference`: stock affine static-factor reference with no
  dependent ScaIR dialects.
- `ordinary_scair_product_with_tail`: ScaIR ordinary index-product route that
  keeps conservative tail cleanup.
- `value_dependent_exact_product`: dependent dynamic route using
  `dtensor.nat.mul`, expected to avoid tail/min cleanup in `d_affine`.
- `value_dependent_static_affine_compatible`: static `nat.const` route expected
  to bridge to stock `affine.for` and pass upstream parse/verify/canonicalize,
  affine loop normalize, and affine loop unroll checks.

## Run

```bash
AFFINE_TILING_SIZE_SET=16x3 \
experiments/affine_tiling_benchmark/build_scair_example.sh
```

Useful environment variables:

- `AFFINE_TILING_SIZE_SET`, comma-separated `K0xK1` entries.
- `AFFINE_TILING_ROUTES`, comma-separated route list or `all`.
- `LLVM_BUILD_DIR` or `MLIR_OPT` for upstream MLIR tools.
- `OUT_DIR` for generated artifacts.

## Outputs

- `out/metrics.csv`
- `out/summary.md`
- `out/route_manifest.md`
- `out/route_manifest.json`
- route-local `.input.mlir`, `.tiled.mlir`, and stock check artifacts

## Interpretation

This benchmark is structural-only. It records parsed operation counts, LOC,
tail/min presence, and stock MLIR compatibility checks. It does not measure
runtime and should not be used as a performance result.
