# shape_reification_benchmark

This experiment is structural/code-generation evidence for value-dependent shape provenance. It is
separate from the matmul exact-tiling benchmark.

Thesis claim:

```text
dependent tensor shape provenance
-> repeated dim queries resolve to shared symbolic nat values
-> repeated m*n size computations become syntactically identical
-> reconcile/canonicalize/CSE/DCE remove redundant shape plumbing
```

The kernel is a small shape fanout chain. Six values are treated as having the same `[m, n]` shape.
Each route queries `dim0` and `dim1`, computes an `m*n` size, and folds the six sizes into one
result. This makes cross-value same-shape equality visible in the metrics: stock MLIR can CSE the
identical SSA case, but it cannot merge dims or size arithmetic across unrelated `tensor<?x?xf32>`
SSA values.

## Variants

- `ordinary_dynamic_shape_identical_ssa`: stock MLIR tensor baseline where all repeated `tensor.dim`
  queries use the same tensor SSA value. This is the fair case where upstream CSE can merge
  syntactically identical queries.
- `ordinary_dynamic_shape_same_shape_different_ssa`: stock MLIR tensor baseline where the benchmark
  contract says six tensors are same-shaped, but the IR only contains different `tensor<?x?xf32>`
  SSA values. Upstream CSE cannot merge these dim queries or the repeated `m*n` arithmetic because
  the equality is not represented
- `dependent_shape_no_elim`: ScaIR dependent tensor route using the same six-argument fanout shape
  as `ordinary_dynamic_shape_same_shape_different_ssa`, but with shared `%m/%n` provenance in the
  tensor argument types. This variant omits the provenance-aware dim-query elimination pass.
- `dependent_shape_dim_elim`: the congruent ScaIR dependent tensor route with
  `dependent-dim-query-elim`, followed by `reconcile-unrealized-casts,canonicalize,cse,dce`.

## Upstream MLIR Baseline

Stock MLIR already has useful local shape cleanup:

- `tensor.dim` / `memref.dim` fold static dimensions and selected producer patterns.
- `tensor::getMixedSize(s)` and `memref::getMixedSize(s)` materialize dynamic dimensions as `dim`
  queries.
- `resolve-shaped-type-result-dims` rewrites dims of op results when producers implement
  `InferShapedTypeOpInterface` or `ReifyRankedShapedTypeOpInterface`.
- `reify-result-shapes` materializes explicit shape results for supported ops.
- CSE removes repeated dim queries only when the operation is syntactically equivalent, such as the
  same source SSA value and same constant dimension.
- Value-bounds reasoning can prove selected equalities in transformations, but ordinary CSE does not
  use a global symbolic shape contract for unrelated tensor SSA values.

The dependent route does not replace those mechanisms. It exposes the same-shape contract earlier
and more directly: `!d_tensor.tensor<[%m, %n], f32>` records that different tensor SSA values share
the same dimensions.

## Run

```bash
experiments/design_benchmarks/shape_reification_benchmark/build_shape_reification_example.sh
```

Useful environment variables:

- `SCAIR_OPT`, default `out/tools/opt/launcher.dest/run`
- `LLVM_BUILD_DIR`, default `$HOME/dev/llvm-clean-build`
- `MLIR_OPT`, default `$LLVM_BUILD_DIR/bin/mlir-opt`
- `OUT_DIR`, default `experiments/design_benchmarks/shape_reification_benchmark/out`

Outputs:

- `experiments/design_benchmarks/shape_reification_benchmark/out/metrics.csv`
- `experiments/design_benchmarks/shape_reification_benchmark/out/summary.md`

## Interpretation

The expected result is not a runtime speedup claim. The benchmark counts dim and shape plumbing
before and after cleanup:

- ordinary identical-SSA dims shrink with upstream CSE
- ordinary different-SSA dims and per-value `m*n` computations remain separate
- dependent dims are rewritten to shared `%m/%n` provenance, after which ordinary cleanup removes
  redundant casts, repeated `size witness erasure` ops, and repeated `m*n` computations
