# Semi-Affine Map Benchmarks

This folder holds the executable semi-affine layout benchmark family. It sits
between the control-flow design benchmarks and the larger GEMM kernels: the
benchmark is still small and readable, but it is already about how layout-heavy
kernels lower and execute.

## What this family measures

The main executable benchmark compares two ScaIR encodings of the same
semi-affine fill-and-sum kernel:

- a baseline route lowered with the baseline dynamic-memref path
- a value-dependent route lowered with the refined path

Both variants operate over a semi-affine reinterpretation of storage and then
produce the same checksum-style scalar result.

The expected thesis argument for this folder is:

- both variants should compute the same observable result
- the value-dependent route should expose layout state more directly in the
  source and/or lowered representation
- any runtime difference is secondary to the structural comparison in this
  family

## Main executable benchmark pair

The primary executable sources are:

- `semi_affine_kernel_scair_baseline_bare.mlir`
- `semi_affine_kernel_scair_bare.mlir`

These are built by `build_scair_example.sh` into the benchmark row reported as:

- benchmark: `semi_affine_fill_and_sum`
- variants: `baseline`, `value_dependent`

## Parser example

The file `semi_affine_indexing_parse_example.mlir` is present in this folder as a
parser/example artifact. It is not part of the main executable comparison pair
and is not currently reported as a row in the unified metrics output.

## Build and run

Build the executable benchmark pair:

```bash
bash experiments/semi_affine_indexing_benchmark/build_scair_example.sh
```

Example runs:

```bash
./experiments/semi_affine_indexing_benchmark/build_scair/semi_affine_baseline_kernel_only_scair_exec 100
./experiments/semi_affine_indexing_benchmark/build_scair/semi_affine_value_dependent_scair_exec 100
```

The executable argument is an optional iteration count for timing.

## Outputs generated

Artifacts are written under `experiments/semi_affine_indexing_benchmark/build_scair/`,
including:

- executable binaries for both compared variants
- lowered LLVM IR (`*.ll`)
- intermediate lowered MLIR (`*.llvm.mlir`)
- key-value runtime outputs (`*_output.txt`)
- `summary.md`
- `metrics.csv`

## Metrics recorded

This family uses the shared schema from `experiments/common_metrics.sh` and
records:

- source bytes and LOC
- source op count
- function count
- block argument count
- memref/layout-related source op counts
- LLVM IR line count
- LLVM call count
- compile time
- runtime
- result and expected result

For the thesis, the most relevant metrics here are usually:

- source op count
- block argument count
- memref/layout-related source op counts
- LLVM IR line count
- compile time

Runtime is still emitted, but it should be interpreted as a supporting metric
for this family rather than the headline claim.
