# Experiments

This directory contains the thesis-facing benchmark and validation suite. The active experiments are
grouped by claim type:

- **Tiling benchmarks**: product/factorization-aware tiling evidence.
- **Structural benchmarks**: lowered IR/code-structure evidence for selected kernels and layouts.
- **Design benchmarks**: language and dependent-shape infrastructure evidence.
- **Archive**: preserved experiments that are not active thesis-facing comparisons.

Shared infrastructure remains at the root of `experiments/`: `build_all_metrics.sh`,
`run_experiments.sh`, `common_metrics.sh`, `compile_time_benchmark.sh`, `run_pipeline.py`,
`collect_env.py`, and `summarize_results.py`.

## Global Requirements

- Build ScaIR first, or let the benchmark scripts use the existing opt launcher.
- `SCAIR_OPT` defaults to `out/tools/opt/launcher.dest/run` in structural scripts.
- Runtime/LLVM benchmarks require an LLVM build, currently tested with
  `LLVM_BUILD_DIR=$HOME/dev/llvm-clean-build`, with `mlir-opt`, `mlir-translate`, and `clang`.
- Structural scripts may use `MLIR_OPT` directly and default to `$LLVM_BUILD_DIR/bin/mlir-opt`.
- Every benchmark writes deterministic family-local outputs under its own `out/` directory unless
  `OUT_DIR` is overridden.

## Tiling Benchmarks

| Experiment                                                  | Command                                                                                                                                                                                                        | Main Claim                                                                                                                                  |
| ----------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| `tiling_benchmarks/affine_tiling_benchmark`                 | `AFFINE_TILING_SIZE_SET=16x3 experiments/tiling_benchmarks/affine_tiling_benchmark/build_scair_example.sh`                                                                                                     | Minimal product-loop case for exact tiling and static affine compatibility.                                                                 |
| `tiling_benchmarks/tail_min_simplifier_benchmark`           | `experiments/tiling_benchmarks/tail_min_simplifier_benchmark/build_tail_min_simplifier_example.sh`                                                                                                             | Dependent `arith.muli` proof removes conservative `min(tile + T, N)` after guarded tiling.                                                     |
| `tiling_benchmarks/tiling_correctness_matrix`               | `experiments/tiling_benchmarks/tiling_correctness_matrix/build_tiling_correctness_matrix.sh`                                                                                                                   | Property matrix for ordinary tails, dependent exact dynamic/static/runtime-checked tiling, negative controls, and nested/commuted products. |
| `tiling_benchmarks/matmul_outer_dim_tiling_benchmark`       | `MATMUL_OUTER_DIM_TILING_ITERATIONS=1 experiments/tiling_benchmarks/matmul_outer_dim_tiling_benchmark/build_matmul_outer_dim_tiling_example.sh`                                                                | Primary apples-to-apples MLIR comparison for `i/j` output-loop tiling.                                                                      |
| `tiling_benchmarks/matmul_reduction_dim_tiling_benchmark`   | `MATMUL_REDUCTION_DIM_TILING_ITERATIONS=1 MATMUL_REDUCTION_DIM_TILING_SIZE_SET=128x128x12x64 experiments/tiling_benchmarks/matmul_reduction_dim_tiling_benchmark/build_matmul_reduction_dim_tiling_example.sh` | Supporting proof benchmark for `K=K0*K1` reduction tiling.                                                                                  |
| `tiling_benchmarks/matmul_full_factorized_tiling_benchmark` | `MATMUL_FULL_FACTORIZED_TILING_ITERATIONS=1 experiments/tiling_benchmarks/matmul_full_factorized_tiling_benchmark/build_matmul_full_factorized_tiling_example.sh`                                              | Composition evidence for simultaneous `M/N/K` product-proof tiling.                                                                         |
| `tiling_benchmarks/conv2d_output_dim_tiling_benchmark`      | `CONV2D_OUTPUT_DIM_TILING_ITERATIONS=1 experiments/tiling_benchmarks/conv2d_output_dim_tiling_benchmark/build_conv2d_output_dim_tiling_example.sh`                                                             | Primary fair Conv2D output-space tiling comparison over `n/cout/oh/ow`.                                                                     |
| `tiling_benchmarks/conv2d_reduction_dim_tiling_benchmark`   | `CONV2D_REDUCTION_DIM_TILING_ITERATIONS=1 experiments/tiling_benchmarks/conv2d_reduction_dim_tiling_benchmark/build_conv2d_reduction_dim_tiling_example.sh`                                                    | Supporting Conv2D reduction evidence for `Cin * Kh * Kw` factorization.                                                                     |
| `tiling_benchmarks/conv2d_full_factorized_tiling_benchmark` | `CONV2D_FULL_FACTORIZED_TILING_ITERATIONS=1 experiments/tiling_benchmarks/conv2d_full_factorized_tiling_benchmark/build_conv2d_full_factorized_tiling_example.sh`                                              | Composition evidence that output and reduction product proofs remove provable guards together.                                              |

## Structural Benchmarks

| Experiment                                             | Command                                                                                                                                                              | Main Claim                                                                                          |
| ------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| `structural_benchmarks/strided_matmul_benchmark`       | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 GEMM_SIZE_SET=128x128x128 experiments/structural_benchmarks/strided_matmul_benchmark/build_scair_example.sh`   | Refined `d_memref` route removes memref descriptor extract/insert plumbing in lowered strided GEMM. |
| `structural_benchmarks/convolution_benchmark`          | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 CONV_SIZE_SET=1x3x32x32x16x3x3 experiments/structural_benchmarks/convolution_benchmark/build_scair_example.sh` | Value-dependent/refined route removes memref descriptor extract/insert plumbing in lowered Conv2D.  |
| `structural_benchmarks/semi_affine_indexing_benchmark` | `BENCH_WARMUP_REPS=0 BENCH_TIMING_REPS=1 ITERATIONS=1 experiments/structural_benchmarks/semi_affine_indexing_benchmark/build_scair_example.sh`                       | Dynamic-strided semi-affine layout lowering and checksum validation.                                |

## Design Benchmarks

| Experiment                                      | Command                                                                                        | Main Claim                                                                                      |
| ----------------------------------------------- | ---------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `design_benchmarks/type_polymorphism`           | `experiments/design_benchmarks/type_polymorphism/build_scair_example.sh`                       | TLam monomorphization, specialization, and erasure as supporting language infrastructure.       |
| `design_benchmarks/shape_reification_benchmark` | `experiments/design_benchmarks/shape_reification_benchmark/build_shape_reification_example.sh` | Dependent shape provenance rewrites repeated dim queries to shared SSA facts, enabling cleanup. |

## Archive

| Experiment                        | Status                                                                                                                                                                                                                                                             |
| --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `archive/attention_mha_benchmark` | Preserved supporting experiment. It demonstrates that `H = heads * head_dim` provenance can guide exact hidden-dimension tiling in an MHA-shaped kernel, but it is not active thesis-facing evidence because the MLIR baseline does not tile the same loop target. |

## Aggregate Runs

Run the active thesis suite:

```bash
experiments/build_all_metrics.sh
```

`run_experiments.sh` wraps the same aggregate runner with thesis-facing default iteration counts.
The archived attention benchmark is intentionally excluded from active aggregate runs.

## Generated Outputs

- `experiments/out/all_metrics.csv`: concatenated common-schema metrics for active runtime/lowering
  families.
- `experiments/out/summary.md`: aggregate summary for `all_metrics.csv`.
- `experiments/out/structural_metrics_manifest.json`: manifest for family-specific structural
  metrics.
- `experiments/out/tiling_benchmarks/`, `experiments/out/structural_benchmarks/`, and
  `experiments/out/design_benchmarks/`: mirrored aggregate copies of per-family metrics and
  summaries.
- `experiments/*/*/out/`: family-local generated artifacts. These are reproducible and may be
  deleted/regenerated.
- `experiments/benchmark_runs/`: saved historical runs. Treat as archival evidence, not current
  source of truth unless the thesis cites the exact run ID.

## Claim Boundaries

Do not use these experiments to claim broad MLIR replacement, general symbolic-shape reasoning,
GPU/vectorization/fusion support, or general runtime superiority. The active claims are narrower:
dependent product facts can prove exact divisibility before lowering, dependent provenance can
remove targeted tail/min guards, dependent shape facts can expose cleanup opportunities, and
selected refined memory-layout routes can reduce lowered descriptor plumbing.
