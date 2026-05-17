# conv2d_reduction_dim_tiling_benchmark

## Purpose

This is supporting Conv2D proof evidence for the reduction domain. It shows that the flattened reduction `Cin * Kh * Kw` can be represented as `Cin0 * (Cin1 * Kh * Kw)` so the compiler tiles by a meaningful full-filter input-channel block instead of merely by `Kw`.

## Kernel Shape

```text
Cin = Cin0 * Cin1
R = Cin * Kh * Kw = Cin0 * (Cin1 * Kh * Kw)

for n, co, oh, ow:
  acc = 0
  for p in 0..R:
    acc += X[...] * K[...]
  Y[n,co,oh,ow] = acc
```

## Routes Table

| Input MLIR | Route | Command / pipeline | Output behavior |
| --- | --- | --- | --- |
| `conv2d_reduction_mlir_baseline.mlir` | `mlir_baseline_reduction_tile` | `mlir-opt --affine-loop-tile=tile-size=Cin1*Kh*Kw` | Upstream reference; may only tile legal outer affine bands. |
| `ordinary_conv2d_reduction_tiling_kernel.mlir` | `ordinary_scair_reduction_tile_with_tail` | `ordinary-affine-product-loop-tile-with-tail:Cin1*Kh*Kw` | Ordinary index-product route keeps min/tail guards. |
| `dependent_conv2d_reduction_tiling_kernel.mlir` | `dependent_reduction_guarded_tail_simplified` | `dependent-tile-with-tail-control`, then `dependent-tail-min-simplify` | Emits guarded reduction chunks, then removes guards by proof. |
| `dependent_conv2d_reduction_tiling_kernel.mlir` | `dependent_reduction_exact_tile` | `dependent-product-loop-exact-tile` | Diagnostic direct exact reduction tiling. |

## What Is Fairly Compared

The ordinary and dependent ScaIR routes target the same flattened reduction loop and the same intended reduction tile, `Cin1 * Kh * Kw`.

## What Is Not Claimed

Reduction tiling alone does not parallelize Conv2D. Parallel reduction would require accumulator privatization and partial-sum lowering. This benchmark is not the primary fair MLIR comparison.

## How To Run

```bash
CONV2D_REDUCTION_DIM_TILING_ITERATIONS=1 \
experiments/conv2d_reduction_dim_tiling_benchmark/build_conv2d_reduction_dim_tiling_example.sh
```

Key variables: `CONV2D_REDUCTION_DIM_TILING_SIZE_SET`, `CONV2D_REDUCTION_DIM_ROUTES`.

## How To Inspect Output

Inspect `out/summary.md`, `out/metrics.csv`, and representative `*.tiled.mlir` files. The guarded route also emits `*.guarded.mlir`.
