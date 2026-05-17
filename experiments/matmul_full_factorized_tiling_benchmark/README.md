# matmul_full_factorized_tiling_benchmark

## Purpose

This is the composition benchmark for factorized matmul tiling. It combines output-space factorization, `M=M0*M1` and `N=N0*N1`, with reduction factorization, `K=K0*K1`, and checks whether the dependent proof route can tile all three loop dimensions and remove all provably unnecessary guards. It is structural evidence that the separate outer-dimension and reduction-dimension ideas compose.

## Kernel Shape

```text
M = M0 * M1
N = N0 * N1
K = K0 * K1

for i in 0..M:
  for j in 0..N:
    for p in 0..K:
      C[i,j] += A[i,p] * B[p,j]
```

```text
Output-space tiling:

C[M,N]
+------------+------------+
| M1 x N1    | M1 x N1    |
| tile       | tile       |
+------------+------------+
| M1 x N1    | M1 x N1    |
| tile       | tile       |
+------------+------------+

Reduction tiling inside each C tile:

K = K0 * K1

p:
0 ...... K1 | K1 ...... 2K1 | ... | (K0-1)K1 ...... K0K1
  K1 tile   |   K1 tile     |     |      K1 tile

Full factorized nest:

for i_outer in 0..M step M1:
  for j_outer in 0..N step N1:
    for i in i_outer..i_outer+M1:
      for j in j_outer..j_outer+N1:
        for p_outer in 0..K step K1:
          for p in p_outer..p_outer+K1:
            accumulate C[i,j]
```

## Routes Table

| Input MLIR | Route | Command / pipeline | Output behavior |
| --- | --- | --- | --- |
| `matmul_full_factorized_mlir_baseline.mlir` | `mlir_baseline_full_tile` | `mlir-opt --affine-loop-tile=tile-size=64` | Upstream MLIR reference. It is labelled as baseline evidence and may only tile legal outer affine bands. |
| `matmul_full_factorized_scair_ordinary.mlir` | `ordinary_scair_full_tile_with_tail` | `ordinary-affine-context-band-tile-with-tail:64`, `ordinary-affine-product-loop-tile-with-tail:64` | Ordinary ScaIR tiles `i/j/p` with static tile sizes and keeps min/tail guards. |
| `matmul_full_factorized_scair_value_dependent.mlir` | `dependent_full_guarded_tail_simplified` | `dependent-context-band-factor-tile-with-tail`, `dependent-tile-with-tail-control`, then `dependent-tail-min-simplify` | Main route: emits guarded factorized tiles for `M/N/K`, then removes all provably unnecessary guards. |
| `matmul_full_factorized_scair_value_dependent.mlir` | `dependent_full_exact_tile` | `dependent-context-band-exact-tile`, `dependent-product-loop-exact-tile` | Diagnostic route that emits tail-free exact tiling directly from the dependent factors. |

## What Is Fairly Compared

The benchmark checks composition on one logical matmul kernel with the same factor facts for `M`, `N`, and `K`. The dependent and ordinary ScaIR routes target `i`, `j`, and `p`; the upstream MLIR route is included as a reference and is explicitly labelled when it only tiles legal outer affine bands.

## What Is Not Claimed

This is not the primary fairness benchmark and not a parallel speedup claim. Tiling the reduction dimension is meaningful for cache, vectorization, staging structure, and exact `K1` chunks, but parallel execution would require a reduction-aware lowering with partial sums.

## How To Run

```bash
MATMUL_FULL_FACTORIZED_TILING_ITERATIONS=1 \
experiments/matmul_full_factorized_tiling_benchmark/build_matmul_full_factorized_tiling_example.sh
```

Key environment variables:

- `MATMUL_FULL_FACTORIZED_TILING_ITERATIONS`: runtime repetitions.
- `MATMUL_FULL_FACTORIZED_TILING_SIZE_SET`: comma-separated `M0xM1xN0xN1xK0xK1` entries.
- `MATMUL_FULL_FACTORIZED_TILE_SIZE`: shared static MLIR/ordinary tile size, default `64`.
- `MATMUL_FULL_FACTORIZED_ROUTES`: comma-separated route list or `all`.

## How To Inspect Output

Inspect `out/summary.md` for the compact route table, `out/metrics.csv` for counts of min/tail operations and dynamic steps, and representative `*.tiled.mlir` files. For the guarded dependent route, compare `*.guarded.mlir` with the final `*.tiled.mlir` to see the proof-based tail removal.
