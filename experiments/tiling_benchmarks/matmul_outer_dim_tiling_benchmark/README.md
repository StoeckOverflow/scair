# matmul_outer_dim_tiling_benchmark

## Purpose

This is the primary apples-to-apples matmul tiling benchmark. It compares upstream MLIR, ordinary
ScaIR, and dependent ScaIR on the same logical kernel and the same tiled loop dimensions: the
output-space loops `i` and `j`. The thesis claim is structural fairness plus stronger dependent
proof power: when `M=M0*M1` and `N=N0*N1`, the dependent route can prove that the `M1` and `N1`
tiles are exact and remove tail guards that stock dynamic affine/index arithmetic keeps.

## Kernel Shape

```text
M = M0 * M1
N = N0 * N1
K = dynamic/fixed benchmark parameter

for i in 0..M:
  for j in 0..N:
    for p in 0..K:
      C[i,j] += A[i,p] * B[p,j]
```

## Routes Table

| Input MLIR                                    | Route                                  | Command / pipeline                                                                            | Output behavior                                                                                                                     |
| --------------------------------------------- | -------------------------------------- | --------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `matmul_outer_dim_mlir_baseline.mlir`         | `mlir_baseline_mn_tile`                | `mlir-opt --affine-loop-tile=tile-size=64`                                                    | Upstream MLIR tiles `i/j` with `step 64` and keeps `affine.min` tail bounds.                                                        |
| `matmul_outer_dim_scair_ordinary.mlir`        | `ordinary_scair_mn_tile_with_tail`     | `ordinary-affine-context-band-tile-with-tail:64`, then stock affine lowering                  | Ordinary ScaIR tiles the same `i/j` loops and keeps min/tail bounds.                                                                |
| `matmul_outer_dim_scair_value_dependent.mlir` | `dependent_mn_guarded_tail_simplified` | `dependent-context-band-factor-tile-with-tail`, then `dependent-tail-min-simplify`            | Emits guarded `i/j` tiles first; final output removes the provably unnecessary guards.                                              |
| `matmul_outer_dim_scair_value_dependent.mlir` | `dependent_mn_separable_tile`          | `dependent-context-band-separable-tile`, then `d-affine-to-affine-compatible` before lowering | Emits full-tile and partial-tile branches for `i/j`; the full branch is exact and the partial branch keeps guarded tail protection. |
| `matmul_outer_dim_scair_value_dependent.mlir` | `dependent_mn_exact_tile`              | `dependent-context-band-exact-tile`                                                           | Diagnostic route that emits tail-free exact `i/j` tiling directly from proof-only dependent facts.                                  |

## What Is Fairly Compared

All routes start from the same matmul loop nest and tile the same output dimensions, `i` and `j`,
using the same default tile factor `64`. The reduction loop `p` is intentionally left structurally
comparable across routes.

## What Is Not Claimed

This benchmark does not claim broad matmul runtime superiority or general polyhedral tiling. Runtime
numbers are sanity checks and supporting measurements; the main claim is that dependent product
facts support a representative affine-style tiling subset where exact routes remove dynamic tail
guards and the separable route exposes full/partial tile control flow in the same tiling target.

## How To Run

```bash
MATMUL_OUTER_DIM_TILING_ITERATIONS=1 \
experiments/tiling_benchmarks/matmul_outer_dim_tiling_benchmark/build_matmul_outer_dim_tiling_example.sh
```

Key environment variables:

- `MATMUL_OUTER_DIM_TILING_ITERATIONS`: runtime repetitions.
- `MATMUL_OUTER_DIM_SIZE_SET`: comma-separated `M0xM1xN0xN1xK` entries.
- `MATMUL_OUTER_DIM_TILE_SIZE`: shared static MLIR/ordinary tile size, default `64`.
- `MATMUL_OUTER_DIM_ROUTES`: comma-separated route list or `all`.

## How To Inspect Output

Inspect `out/summary.md` for the compact route table, `out/metrics.csv` for counts of `affine.min`,
`arith.minsi`, `d_affine.for`, `affine.for`, and dynamic steps, and representative `*.tiled.mlir`
files for the actual tiled IR.
