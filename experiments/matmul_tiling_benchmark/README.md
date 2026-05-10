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
- core reduction-tiling benchmark for direct product provenance $K=K_0K_1$
- compares ordinary dynamic $K$ arithmetic with value-dependent $dtensor.nat.mul$ provenance
- value-dependent LLVM IR is shape-generic, so per-size `.ll` artifacts may be identical and are kept per row for traceability

Variant notes:
- `mlir_baseline`: upstream MLIR affine tiling baseline
- `scair_baseline`: ScaIR dynamic memref baseline without dependent provenance or tiling
- `value_dependent`: ScaIR dependent route with factorization-aware tiling
- `value_dependent_guarded_tile_tail_simplified`: ScaIR dependent route that
  first emits the same conservative guarded tiling shape as the tail-control
  tiler, then consumes `dtensor.nat.mul` provenance with
  `dependent-tail-min-simplify` to remove the generated tail guard.
- `value_dependent_exact_tile`: dynamic ScaIR staging route; preserves
  `dtensor.nat.mul`, tiles by the runtime RHS factor, and demonstrates no
  reduction-tail cleanup inside `d_affine`
- `value_dependent_static_affine_compatible_unroll`: static bridge route; uses
  `nat.const` for the RHS factor, lowers eligible `d_affine` loops to stock
  `affine.for`, checks upstream `mlir-opt` parse/canonicalize/normalize/unroll
  with default `unroll-factor=4`, and records full K1 unroll separately when it
  succeeds
- `MATMUL_TILING_PROFILE=cache_sweep`: opt-in cache-sensitive profile that sweeps explicit MLIR tile sizes with `MATMUL_TILING_TILE_SIZE_SET`
- if added later, `scair_baseline_tiled` should be documented as the ordinary ScaIR tiling control so the comparison separates baseline lowering, generic tiling, and dependent factorization-aware tiling
- deterministic input values are chosen so the numeric result/checksum is non-negative

Use in thesis:
- structural/code-generation evidence with supporting runtime validation
- not a broad matmul speedup claim or proof of general product solving
- copy-ready thesis text for the dependent `dtensor.nat.mul` proof and the
  latest benchmark result lives in
  `experiments/matmul_tiling_benchmark/dependent_natmul_thesis_section.md`

Metric interpretation / limitations:
- `scair_baseline` is the ScaIR dynamic memref baseline and intentionally does not carry dependent `dtensor.nat.mul` provenance.
- The current `scair_baseline` route is also intentionally untiled; do not describe this benchmark as a comparison against an ordinary tiled ScaIR baseline until a separate `scair_baseline_tiled` control exists.
- `cache_sweep` is cache-sensitive rather than hardware-optimal: tile sizes are explicit experiment parameters, not compiler-selected cache-optimal choices.
- In `cache_sweep`, `mlir_baseline` emits one row per explicit tile size, while `scair_baseline` and `value_dependent` emit one row per problem size.
- `tail_handling_present`, `factorized_tile_count`, and `tail_free_factorized` are artifact checks over generated `.tiled.mlir`, not semantic proofs.
- `value_dependent_guarded_tile_tail_simplified` records
  `guarded_tail_handling_present` in the notes so the benchmark shows the
  before/after distinction: the guarded stage emits a tail and the simplified
  stage removes it using dependent product facts.
- The static affine-compatible route is a downstream optimization bridge
  artifact, not a broad runtime-superiority claim. Its core metrics are the
  stock MLIR checks and absence of `affine.min` / `arith.minsi` / dynamic steps.

Example cache-sensitive run:

```bash
MATMUL_TILING_PROFILE=cache_sweep \
MATMUL_TILING_TILE_SIZE_SET=8,16,32,64,128 \
BENCH_CPU_PIN=0 \
bash experiments/matmul_tiling_benchmark/build_scair_example.sh
```

Fast thesis smoke run:

```bash
MATMUL_TILING_ITERATIONS=1 \
MATMUL_TILING_SIZE_SET=128x128x12x64 \
experiments/matmul_tiling_benchmark/build_scair_example.sh
```

Useful environment variables:
- `MATMUL_TILING_ROUTES`: comma-separated route list or `all`
- `MATMUL_TILING_SIZE_SET`: comma-separated `MxNxK0xK1` entries
- `MATMUL_TILING_PROFILE`: `default`, `cache_control`, or `cache_sweep`
- `MATMUL_TILING_TILE_SIZE_SET`: explicit MLIR tile sizes for `cache_sweep`
- `MATMUL_TILING_ITERATIONS` / `ITERATIONS`
- `LLVM_BUILD_DIR`, `MLIR_OPT`, `MLIR_TRANSLATE`, `CC`, `OUT_DIR`

Outputs:
- `out/metrics.csv`
- `out/summary.md`
- route-local MLIR/LLVM artifacts, executables, output logs, raw timings, and
  stock-MLIR compatibility artifacts for the static affine-compatible route
