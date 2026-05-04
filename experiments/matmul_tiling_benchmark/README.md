# matmul_tiling_benchmark

$$
K = K_0K_1
$$

$$
C_{i,j}
=
\sum_{p=0}^{K-1}
A_{i,p}\,B_{p,j}
\qquad
0\leq i<M,\;0\leq j<N
$$

Tests:
- supporting reduction-tiling benchmark for direct product provenance $K=K_0K_1$
- compares ordinary dynamic $K$ arithmetic with value-dependent $dtensor.nat.mul$ provenance
- value-dependent LLVM IR is shape-generic, so per-size `.ll` artifacts may be identical and are kept per row for traceability

Variant notes:
- `mlir_baseline`: upstream MLIR affine tiling baseline
- `scair_baseline`: ScaIR dynamic memref baseline without dependent provenance or tiling
- `value_dependent`: ScaIR dependent route with factorization-aware tiling
- if added later, `scair_baseline_tiled` should be documented as the ordinary ScaIR tiling control so the comparison separates baseline lowering, generic tiling, and dependent factorization-aware tiling
- deterministic input values are chosen so the numeric result/checksum is non-negative

Use in thesis:
- structural/code-generation evidence with supporting runtime validation
- not a broad matmul speedup claim or proof of general product solving

Metric interpretation / limitations:
- `scair_baseline` is the ScaIR dynamic memref baseline and intentionally does not carry dependent `dtensor.nat.mul` provenance.
- The current `scair_baseline` route is also intentionally untiled; do not describe this benchmark as a comparison against an ordinary tiled ScaIR baseline until a separate `scair_baseline_tiled` control exists.
- `tail_handling_present`, `factorized_tile_count`, and `tail_free_factorized` are artifact checks over generated `.tiled.mlir`, not semantic proofs.
