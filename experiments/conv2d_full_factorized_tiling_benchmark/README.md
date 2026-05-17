# conv2d_full_factorized_tiling_benchmark

## Purpose

This is the Conv2D composition benchmark. It combines output-space product facts with reduction-domain product facts and checks that dependent tiling can remove guards across both parts of the loop nest.

## Kernel Shape

```text
N = N0 * N1
Cout = Cout0 * Cout1
OH = OH0 * OH1
OW = OW0 * OW1
Cin = Cin0 * Cin1
R = Cin0 * (Cin1 * Kh * Kw)
```

```text
Output tile:
  N1 x Cout1 x OH1 x OW1

Reduction tile inside each output tile:
  Cin1 x Kh x Kw

parallelizable later:
  output tile axes

reduction-aware later:
  reduction tile axis with partial sums
```

## Routes Table

| Input MLIR | Route | Command / pipeline | Output behavior |
| --- | --- | --- | --- |
| `conv2d_full_factorized_mlir_baseline.mlir` | `mlir_baseline_full_tile` | `mlir-opt --affine-loop-tile=tile-size=8` | Upstream reference; labelled if it only tiles legal outer affine bands. |
| `conv2d_full_factorized_scair_ordinary.mlir` | `ordinary_scair_full_tile_with_tail` | `ordinary-affine-context-band-tile-with-tail:8`, `ordinary-affine-product-loop-tile-with-tail:Cin1*Kh*Kw` | Ordinary route tiles output and reduction loops, keeping guards. |
| `conv2d_full_factorized_scair_value_dependent.mlir` | `dependent_full_guarded_tail_simplified` | `dependent-context-band-factor-tile-with-tail`, `dependent-tile-with-tail-control`, then `dependent-tail-min-simplify` | Main composition route; final IR removes provable guards. |
| `conv2d_full_factorized_scair_value_dependent.mlir` | `dependent_full_exact_tile` | `dependent-context-band-exact-tile`, `dependent-product-loop-exact-tile` | Diagnostic direct exact full tiling. |

## What Is Fairly Compared

The ordinary and dependent routes target both output-space loops and the flattened reduction loop. The MLIR route is an upstream reference and is not the primary full-factorized fairness claim if it tiles only legal outer bands.

## What Is Not Claimed

This benchmark does not claim automatic parallel Conv2D lowering or broad runtime superiority. Reduction tiles expose exact chunks, but parallel execution still needs partial sums.

## How To Run

```bash
CONV2D_FULL_FACTORIZED_TILING_ITERATIONS=1 \
experiments/conv2d_full_factorized_tiling_benchmark/build_conv2d_full_factorized_tiling_example.sh
```

Key variables: `CONV2D_FULL_FACTORIZED_TILING_SIZE_SET`, `CONV2D_FULL_FACTORIZED_OUTPUT_TILE_SIZE`, `CONV2D_FULL_FACTORIZED_ROUTES`.

## How To Inspect Output

Inspect `out/summary.md`, `out/metrics.csv`, and representative `*.tiled.mlir` files. The guarded route also emits `*.guarded.mlir`.
