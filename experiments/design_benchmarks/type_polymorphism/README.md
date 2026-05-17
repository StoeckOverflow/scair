# Type Polymorphism Design Benchmark

This is a small design/infrastructure benchmark for ScaIR's TLam
type-polymorphism pipeline. It supports the thesis story by showing that
polymorphic IR can be specialized, erased, and lowered to ordinary `func` IR
with no residual polymorphic/type-level TLam constructs.

It is not a central runtime or performance benchmark. The main experimental
emphasis remains on matmul, convolution, semi-affine indexing, and the
tiling/provenance experiments.

## Cases

- `polymorphic_identity_specialization`: one polymorphic identity specialized
  at `i32` and `i64`, measured for both the SSA-in-types TLam route and the
  de Bruijn baseline route.
- `tensor_shape_identity`: the same value-dependent TLam route specialized at
  `tensor<4xi32>`, keeping a small shaped-type example without turning this
  into a tensor runtime benchmark.

The broader TLam and TLam de Bruijn FileCheck suites remain the compiler
regression safety net. This experiment is only the thesis-facing presentation.

## Run

```bash
experiments/design_benchmarks/type_polymorphism/build_scair_example.sh
```

Useful environment variables:

- `SCAIR_OPT`, default `out/tools/opt/launcher.dest/run`.
- `OUT_DIR`, default `experiments/design_benchmarks/type_polymorphism/out`.

## Outputs

- `out/metrics.csv`: common-schema structural row for aggregate experiment
  collection.
- `out/design_metrics.csv`: focused design metrics with input/output op count,
  generated specializations, leftover polymorphic ops, leftover TLam ops, IR
  line counts, pass status, and artifact paths.
- `out/metrics.json`: JSON form of the focused design metrics.
- `out/summary.md`: compact human-readable summary.
- per-case monomorphized and erased/lowered MLIR artifacts.

## Interpretation

Use this benchmark as evidence that type polymorphism is viable supporting
infrastructure on the path toward dependent typing, monomorphization,
specialization, and proof erasure. Do not cite it as evidence for runtime
speedups, general tensor optimization, shape-product reasoning, exact tiling,
tail/min removal, affine bridge compatibility, vectorization, or GPU lowering.
