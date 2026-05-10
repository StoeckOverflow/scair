# Experiments

This directory contains the thesis-facing benchmark and validation suite. The
experiments are intentionally split into:

- **Core thesis evidence**: the main value-dependent optimization claims.
- **Supporting validation**: execution/lowering coverage for related ScaIR
  features and application kernels.
- **Exploratory/future work**: useful context, but not a headline thesis claim.
- **Generated artifacts**: reproducible outputs from scripts, not source inputs.

For the focused thesis story, start with
[`thesis_evaluation_summary.md`](thesis_evaluation_summary.md).
For stable tiling route names and claim boundaries, see
[`tiling_benchmark_routes.md`](tiling_benchmark_routes.md). The pass-level
compiler invariants are documented in
[`../passes/docs/tiling_invariants.md`](../passes/docs/tiling_invariants.md).

## Global Requirements

- Build ScaIR first, or let `./mill -i filechecks.run` build the opt launcher.
- `SCAIR_OPT` defaults to `out/tools/opt/launcher.dest/run` in structural
  scripts.
- Runtime/LLVM benchmarks require an LLVM build, currently tested with
  `LLVM_BUILD_DIR=$HOME/dev/llvm-clean-build`, with `mlir-opt`,
  `mlir-translate`, and `clang`.
- Structural scripts `shape_reification_benchmark` and
  `tail_min_simplifier_benchmark` use `MLIR_OPT` directly and default to
  `$LLVM_BUILD_DIR/bin/mlir-opt`.
- Every script writes deterministic outputs under its family-local `out/`
  directory unless `OUT_DIR` is overridden.

## Experiment Index

| Experiment | Class | Command | Outputs | Main Claim | Correctness |
|---|---|---|---|---|---|
| `matmul_tiling_benchmark` | Core thesis evidence | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 MATMUL_TILING_ITERATIONS=1 MATMUL_TILING_SIZE_SET=128x128x12x64 experiments/matmul_tiling_benchmark/build_scair_example.sh` | common-schema `metrics.csv`, `summary.md`, IR/LLVM/runtime artifacts | `dtensor.nat.mul` proves exact K tiling; static `nat.const` route bridges to stock affine normalize/unroll with no cleanup | Runtime checksum/result |
| `shape_reification_benchmark` | Core thesis evidence | `experiments/shape_reification_benchmark/build_shape_reification_example.sh` | structural `metrics.csv`, `summary.md`, before/after IR | dependent shape provenance rewrites repeated dim queries to shared SSA facts, enabling CSE/DCE compaction | Structural only |
| `tail_min_simplifier_benchmark` | Core thesis evidence | `experiments/tail_min_simplifier_benchmark/build_tail_min_simplifier_example.sh` | structural `metrics.csv`, `summary.md`, guarded/simplified IR | dependent `nat.mul` proof removes conservative `min(tile + T, N)` after guarded tiling | Structural only |
| `tiling_correctness_matrix` | Core thesis evidence | `experiments/tiling_correctness_matrix/build_tiling_correctness_matrix.sh` | structural `metrics.csv`, `metrics.json`, `summary.md`, route manifests, per-case IR | compact property matrix for ordinary tails, dependent exact dynamic/static/runtime-checked tiling, zero negative control, and nested/commuted product matching | Structural validation |
| `conv_tiling_benchmark` | Core thesis evidence | `experiments/conv_tiling_benchmark/build_conv_tiling_example.sh` | structural `metrics.csv`, `metrics.json`, `summary.md`, route manifests, per-case IR | full Conv2D kernel with flattened `Ci * Kh * Kw` reduction shows exact tiling generalizes beyond matmul while ordinary index arithmetic keeps a tail | Structural validation |
| `strided_matmul_benchmark` | Core thesis evidence | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 GEMM_SIZE_SET=128x128x128 experiments/strided_matmul_benchmark/build_scair_example.sh` | common-schema `metrics.csv`, `summary.md`, IR/LLVM/runtime artifacts | selected refined `d_memref` route removes memref descriptor extract/insert plumbing in lowered strided GEMM | Runtime checksum/result plus structural descriptor evidence |
| `convolution_benchmark` | Core thesis evidence | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 CONV_SIZE_SET=1x3x32x32x16x3x3 experiments/convolution_benchmark/build_scair_example.sh` | common-schema `metrics.csv`, `summary.md`, IR/LLVM/runtime artifacts | selected value-dependent/refined route removes memref descriptor extract/insert plumbing in lowered Conv2D | Runtime checksum/result plus structural descriptor evidence |
| `affine_tiling_benchmark` | Supporting validation | `AFFINE_TILING_SIZE_SET=16x3 experiments/affine_tiling_benchmark/build_scair_example.sh` | common-schema `metrics.csv`, `summary.md`, stock-MLIR check artifacts | minimal product-loop case for exact tiling and static affine compatibility | Structural only |
| `attention_mha_benchmark` | Supporting validation | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 ATTENTION_MHA_ROUTES=value_dependent_exact_tile experiments/attention_mha_benchmark/build_scair_example.sh` | common-schema `metrics.csv`, `summary.md`, IR/LLVM/runtime artifacts | application-scale validation that head/head-dim product provenance can guide an exact hidden-dim tile | Runtime checksum/result |
| `semi_affine_indexing_benchmark` | Supporting validation | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 experiments/semi_affine_indexing_benchmark/build_scair_example.sh` | common-schema `metrics.csv`, `summary.md`, IR/LLVM/runtime artifacts | dynamic-strided semi-affine layout lowering and checksum validation | Runtime checksum/result |
| `type_polymorphism` | Supporting validation | `experiments/type_polymorphism/build_scair_example.sh` | common-schema `metrics.csv`, focused `design_metrics.csv`, `metrics.json`, `summary.md`, monomorphized/lowered IR | design/infrastructure benchmark for TLam monomorphization, specialization, and erasure; separate from shape/product optimization | Structural pipeline checks |
| `build_all_metrics.sh` | Aggregate runner | `MATMUL_TILING_ITERATIONS=1 ITERATIONS=1 experiments/build_all_metrics.sh` | `experiments/out/all_metrics.csv`, `summary.md`, env snapshot, `structural_metrics_manifest.json`, `structural/` | concatenates common-schema benchmark families and archives structural tiling evidence separately | Depends on family |

## Generated / Archive-Like Directories

- `experiments/out/`: aggregate generated output from `build_all_metrics.sh`.
- `experiments/*/out/`: family-local generated outputs. These are reproducible
  and may be deleted/regenerated. Scripts overwrite `metrics.csv` and
  `summary.md`, and tiling scripts also write `route_manifest.md` plus
  `route_manifest.json`;
  route-local binaries/logs/IR for earlier size sets may remain until the
  directory is cleaned, so treat the current CSV and summary as the source of
  truth for a run.
- `experiments/benchmark_runs/`: saved historical benchmark runs. Treat as
  archival evidence, not current source of truth unless the thesis cites the
  exact run ID.
- `experiments/__pycache__/`: Python cache; generated, not part of evaluation.

## Metric Schema

Most runtime/lowering benchmarks emit the shared schema from
[`common_metrics.sh`](common_metrics.sh), so `build_all_metrics.sh` can
concatenate them. Structural microbenchmarks currently use narrower
family-specific schemas, so the aggregate runner stores them under
`experiments/out/structural/` instead of appending them to `all_metrics.csv`:

- `shape_reification_benchmark`: dim/query/shape-management operation counts.
- `tail_min_simplifier_benchmark`: min/tail and loop-step operation counts.
- `tiling_correctness_matrix` and `conv_tiling_benchmark`: tiling route,
  proof, loop-step, and tail-bound operation counts.

Use `NA` for metrics that are not applicable. For thesis-facing comparisons,
prefer structural parsed-op counts where available over textual operation
estimates.

## Claim Boundaries

Do not use these experiments to claim:

- general MLIR replacement;
- full symbolic-shape reasoning;
- broad vectorization, GPU, fusion, bufferization, StableHLO, or TOSA support;
- general runtime superiority across kernels or hardware.

The implemented claims are narrower:

- dependent nat/product facts can prove exact divisibility before lowering;
- static `nat.const` product factors can be bridged to stock affine-compatible
  loop forms;
- dependent shape provenance can make repeated shape facts syntactically
  available to cleanup passes;
- dependent provenance can remove conservative tail/min guards in targeted
  generated loops;
- selected refined memory-layout routes can avoid unnecessary memref descriptor
  extract/insert plumbing in lowered kernel IR;
- selected runtime benchmarks validate correctness and lowering viability.
- TLam type polymorphism is supporting language infrastructure for the path
  toward dependent typing and proof erasure, not a central runtime/performance
  benchmark.
