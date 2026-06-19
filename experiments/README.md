# Experiments

This directory contains the thesis-facing benchmark and validation suite. The active experiments are
grouped by claim type:

- **Structural benchmarks**: lowered IR/code-structure evidence for selected kernels and layouts.
- **Design benchmarks**: language and dependent-shape infrastructure evidence.

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

## Aggregate Runs

Run the active thesis suite:

```bash
experiments/build_all_metrics.sh
```

`run_experiments.sh` wraps the same aggregate runner with thesis-facing default iteration counts.

## Generated Outputs

- `experiments/out/all_metrics.csv`: concatenated common-schema metrics for active runtime/lowering
  families.
- `experiments/out/summary.md`: aggregate summary for `all_metrics.csv`.
- `experiments/out/structural_metrics_manifest.json`: manifest for family-specific structural
  metrics.
- `experiments/out/structural_benchmarks/` and `experiments/out/design_benchmarks/`:
  mirrored aggregate copies of per-family metrics and
  summaries.
- `experiments/*/*/out/`: family-local generated artifacts. These are reproducible and may be
  deleted/regenerated.
- `experiments/benchmark_runs/`: saved historical runs. Treat as archival evidence, not current
  source of truth unless the thesis cites the exact run ID.

## Claim Boundaries

Do not use these experiments to claim broad MLIR replacement, general symbolic-shape reasoning,
GPU/vectorization/fusion support, or general runtime superiority. The active claims are narrower:
dependent shape facts can expose cleanup opportunities, and selected refined memory-layout routes
can reduce lowered descriptor plumbing.
