# GEMM Kernel Benchmarks

This folder holds the kernel-oriented matrix-multiplication benchmark family in
the ScaIR repository. Its role in the thesis story is different from the pure
design benchmarks: this family is meant to compare executable kernels and the
quality of the lowered backend structure.

## What this family measures

The current unified benchmark path measures a split GEMM pipeline:

- `matmul_kernel*` computes `C = A * B`
- `checksum_kernel*` reduces the result matrix to one scalar checksum

The benchmark reports one logical executable row, `matmul_checksum`, so the
suite compares the whole kernel pair rather than only the multiplication kernel
in isolation.

This family is therefore most useful for the thesis dimensions:

- optimization / backend quality
- end-to-end executable structure
- compile-time and runtime comparison for the same mathematical workload

## Compared variants

The current unified build path is `build_scair_example.sh`. It compares three
logical variants:

- `mlir_baseline`
  - upstream MLIR kernels lowered through the standard MLIR pipeline
- `baseline`
  - ScaIR baseline kernel-only sources lowered through the baseline dynamic
    memref route
- `value_dependent`
  - ScaIR value-dependent kernel-only sources lowered through the refined route

The expected thesis argument for this folder is:

- ScaIR variants should stay comparable to the upstream MLIR executable at the
  observable result level
- the value-dependent route may improve source and lowered structure relative to
  the ScaIR baseline route
- backend quality should be judged primarily through LLVM size/call structure,
  compile time, and runtime for the same checksum-producing executable

## Legacy vs current build paths

This folder still contains two build entrypoints:

- `build_scair_example.sh`
  - current unified benchmark path
  - emits `summary.md` and `metrics.csv`
  - participates in `experiments/build_all_metrics.sh`
- `build_example.sh`
  - legacy script
  - primarily preserves the older upstream MLIR tiled-vs-untiled comparison
  - does not participate in the current shared metrics schema

The tiled-vs-untiled upstream comparison is therefore legacy-only right now. It
is not part of the current unified benchmark-suite story unless it is later
folded into the shared schema as a separate benchmark row.

## Build and run

Build the unified GEMM benchmark family:

```bash
bash experiments/matmul_checksum_benchmark/build_scair_example.sh
```

The executables take:

- `n`
- `m`
- `k`
- optional iteration count

Example runs:

```bash
./experiments/matmul_checksum_benchmark/build_scair/matmul_mlir_baseline_exec 32 32 32 10
./experiments/matmul_checksum_benchmark/build_scair/matmul_baseline_kernel_only_scair_exec 32 32 32 10
./experiments/matmul_checksum_benchmark/build_scair/matmul_value_dependent_scair_exec 32 32 32 10
```

## Outputs generated

The unified build writes artifacts under `experiments/matmul_checksum_benchmark/build_scair/`,
including:

- executable binaries for the three compared variants
- lowered LLVM IR (`*.ll`)
- intermediate lowered MLIR (`*.llvm.mlir`)
- per-run key-value outputs (`*_output.txt`)
- `summary.md`
- `metrics.csv`

The legacy script continues to write into `experiments/matmul_checksum_benchmark/build/`.

## Metrics recorded

The unified GEMM build participates in the shared experiment schema from
`experiments/common_metrics.sh`. Reported metrics include:

- source bytes and LOC
- source op count
- function count
- block argument count
- memref/layout-related source op counts
- LLVM IR line count
- LLVM call count
- compile time
- runtime via the common key-value driver output
- checksum result and expected result

For this family, the most relevant thesis-facing metrics are usually:

- LLVM IR line count
- LLVM call count
- compile time
- runtime

Source-size metrics are still emitted, but they are usually secondary to the
kernel/backend story.
