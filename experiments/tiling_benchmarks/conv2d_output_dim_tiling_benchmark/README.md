# conv2d_output_dim_tiling_benchmark

## Purpose

This is the primary fair Conv2D tiling comparison. Upstream MLIR, ordinary ScaIR, and dependent ScaIR all tile the same output-space loops: `n`, `cout`, `oh`, and `ow`. The dependent route uses product facts for `N`, `Cout`, `OH`, and `OW` to remove conservative output-tail guards.

## Kernel Shape

```text
N = N0 * N1
Cout = Cout0 * Cout1
OH = OH0 * OH1
OW = OW0 * OW1

for n in 0..N:
  for co in 0..Cout:
    for oh in 0..OH:
      for ow in 0..OW:
        acc = sum over Cin * Kh * Kw
        Y[n,co,oh,ow] = acc
```

## Routes Table

| Input MLIR | Route | Command / pipeline | Output behavior |
| --- | --- | --- | --- |
| `conv2d_output_mlir_baseline.mlir` | `mlir_baseline_output_tile` | `mlir-opt --affine-loop-tile=tile-size=8` | Upstream MLIR tiles output loops and keeps conservative tail bounds. |
| `conv2d_output_scair_ordinary.mlir` | `ordinary_scair_output_tile_with_tail` | `ordinary-affine-context-band-tile-with-tail:8` | Ordinary ScaIR tiles the same output loops and keeps min/tail guards. |
| `conv2d_output_scair_value_dependent.mlir` | `dependent_output_guarded_tail_simplified` | `dependent-context-band-factor-tile-with-tail`, then `dependent-tail-min-simplify` | Emits guarded output tiles, then removes guards by product proof. |
| `conv2d_output_scair_value_dependent.mlir` | `dependent_output_exact_tile` | `dependent-context-band-exact-tile` | Diagnostic direct exact output tiling. |

## What Is Fairly Compared

All routes target the same output-space loops. These loops are directly relevant to later parallel scheduling because different output tiles write disjoint `Y` regions.

## What Is Not Claimed

This benchmark does not claim full Conv2D runtime superiority, cache tuning, vectorization, or automatic parallel lowering.

## How To Run

```bash
CONV2D_OUTPUT_DIM_TILING_ITERATIONS=1 \
experiments/tiling_benchmarks/conv2d_output_dim_tiling_benchmark/build_conv2d_output_dim_tiling_example.sh
```

Key variables: `CONV2D_OUTPUT_DIM_TILING_SIZE_SET`, `CONV2D_OUTPUT_DIM_TILE_SIZE`, `CONV2D_OUTPUT_DIM_ROUTES`.

## How To Inspect Output

Inspect `out/summary.md`, `out/metrics.csv`, and representative `*.tiled.mlir` files. The guarded route also emits `*.guarded.mlir`.
