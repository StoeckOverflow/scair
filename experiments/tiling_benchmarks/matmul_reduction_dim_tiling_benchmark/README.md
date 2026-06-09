# matmul_reduction_dim_tiling_benchmark

## Purpose

This is a supporting proof-directed benchmark for the matmul reduction dimension. It focuses on
`K=K0*K1` and asks whether the compiler can expose exact reduction chunks of width `K1`. It is
useful thesis evidence for dependent factorization over reductions, but it is not the main fair
MLIR-vs-dependent matmul tiling comparison; use `matmul_outer_dim_tiling_benchmark` for that.

## Kernel Shape

```text
M = ordinary benchmark parameter
N = ordinary benchmark parameter
K = K0 * K1

for i in 0..M:
  for j in 0..N:
    for p in 0..K:
      C[i,j] += A[i,p] * B[p,j]
```

## Routes Table

| Input MLIR                                        | Route                                          | Command / pipeline                                                             | Output behavior                                                                                                                    |
| ------------------------------------------------- | ---------------------------------------------- | ------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------- |
| `matmul_kernel_mlir_baseline.mlir`                | `mlir_baseline`                                | `mlir-opt --affine-loop-tile`, using `MATMUL_REDUCTION_DIM_TILING_TILE_POLICY` | Upstream MLIR baseline over legal affine bands; it sees `%k = arith.muli %k0, %k1` but has no dependent provenance proof.          |
| `matmul_kernel_scair_ordinary_index_refined.mlir` | `ordinary_scair_k_tile_with_tail`              | `ordinary-affine-product-loop-tile-with-tail:K1`, then stock affine lowering   | Ordinary ScaIR control route for the same reduction bound; keeps tail/min guards because the product is ordinary index arithmetic. |
| `matmul_kernel_scair_value_dependent.mlir`        | `value_dependent`                              | `dependent-product-loop-exact-tile`, `dependent-size-product-loop-factorization`     | Exact diagnostic route for the product-structured `K` loop.                                                                        |
| `matmul_kernel_scair_value_dependent.mlir`        | `value_dependent_guarded_tile_tail_simplified` | `dependent-tile-with-tail-control`, then `dependent-tail-min-simplify`         | Main proof route: emits a guarded reduction tile, then removes the tail/min guard by proving `K=K0*K1`.                            |

## What Is Fairly Compared

The benchmark fairly compares proof behavior around the reduction bound `K=K0*K1`. It shows how
dependent product information affects reduction-loop tiling artifacts, especially tail/min removal
after guarded tiling.

## What Is Not Claimed

This is not a full matmul tiling comparison against upstream MLIR. It does not claim that the
dependent reduction route is a better whole-kernel tiling strategy than stock MLIR, and it does not
claim parallel speedup for reductions without a separate partial-sum lowering.

## How To Run

```bash
MATMUL_REDUCTION_DIM_TILING_ITERATIONS=1 \
MATMUL_REDUCTION_DIM_TILING_SIZE_SET=128x128x12x64 \
experiments/tiling_benchmarks/matmul_reduction_dim_tiling_benchmark/build_matmul_reduction_dim_tiling_example.sh
```

Key environment variables:

- `MATMUL_REDUCTION_DIM_TILING_ITERATIONS`: runtime repetitions.
- `MATMUL_REDUCTION_DIM_TILING_SIZE_SET`: comma-separated `MxNxK0xK1` entries.
- `MATMUL_REDUCTION_DIM_TILING_ROUTES`: comma-separated route list or `all`.
- `MATMUL_REDUCTION_DIM_TILING_PROFILE`: `default`, `cache_control`, or `cache_sweep`.
- `MATMUL_REDUCTION_DIM_TILING_TILE_POLICY`: `inner_factor` or `fixed32`.
- `MATMUL_REDUCTION_DIM_TILING_TILE_SIZE_SET`: explicit MLIR tile sizes for `cache_sweep`.

## How To Inspect Output

Inspect `out/summary.md` for runtime/build rows, `out/metrics.csv` for common-schema measurements
and route notes, and representative `*.tiled.mlir` files for the exact reduction tiling artifacts.
The guarded route also writes a `*.guarded.mlir` file so the pre-simplification tail can be compared
with the final simplified output.
