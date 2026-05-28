# Representative Benchmark IR Snapshot

This benchmark run stores representative MLIR artifacts only. It intentionally does not preserve
performance numbers as thesis evidence. The purpose is to provide stable IR examples for each
benchmark: source/input forms, key transformation phases, and selected lowered LLVM-dialect MLIR
where the claim is about descriptor lowering.

## Design Benchmarks

### `design_benchmarks/type_polymorphism`

Saved files:

- source programs for the value-dependent identity, de Bruijn identity, and tensor-shape identity
  cases
- `*.monomorphized.mlir`
- `*.erased_lowered.mlir`

Decision: these are the representative phases for the type-polymorphism claim: the source contains
TLam polymorphism, monomorphized IR shows specialization, and erased/lowered IR demonstrates that no
TLam constructs remain.

### `design_benchmarks/shape_reification_benchmark`

Saved files:

- ordinary same-SSA, ordinary different-SSA, and dependent-shape sources
- stock cleanup outputs for ordinary variants
- dependent `after_dim_elim`, `after_dim_elim_cleanup`, and `no_elim_cleanup` outputs

Decision: these files show the contrast between ordinary syntactic CSE and dependent same-shape
provenance. The dependent before/after cleanup phases are the key evidence.

## Tiling Benchmarks

### `tiling_benchmarks/affine_tiling_benchmark`

Saved files:

- all source `.mlir` inputs
- all generated `.tiled.mlir` outputs
- stock-affine compatibility phases: `stock_parse`, `stock_canonicalize`, `stock_normalize`, and
  `stock_unroll`

Decision: this benchmark needs all five routes because each route has a different role: stock
dynamic baseline, stock static reference, ordinary ScaIR control, dependent exact dynamic tiling,
and dependent static affine-compatible bridging.

### `tiling_benchmarks/tail_min_simplifier_benchmark`

Saved files:

- stock, ordinary, and dependent source loops
- stock upstream cleanup output
- ordinary cleanup output
- dependent guarded-tail-simplified output

Decision: these are the three congruent routes needed to show that only the dependent product proof
removes the tail/min guard.

### `tiling_benchmarks/tiling_correctness_matrix`

Saved files:

- all source cases
- all generated `.input.mlir` and `.tiled.mlir` case outputs

Decision: this benchmark is a property matrix, so each case is representative: ordinary tail,
non-divisible ordinary, exact dynamic/static, runtime-checked dynamic, zero negative control,
nested/commuted products, lazy product facts, and lazy tail-product simplification.

### `tiling_benchmarks/matmul_outer_dim_tiling_benchmark`

Saved files:

- source routes for MLIR, ordinary ScaIR, and dependent ScaIR
- representative default row `2_64_2_64_768_*`
- `.input.mlir`, `.tiled.mlir`, `.guarded.mlir`, and `.llvm.mlir` where present

Decision: one default row is enough because the benchmark claim is structural: all routes tile the
same `i/j` output loops. The saved row shows stock/ordinary tails, dependent guarded form, dependent
simplified form, exact form, and lowered MLIR for the executable routes.

### `tiling_benchmarks/matmul_reduction_dim_tiling_benchmark`

Saved files:

- source routes for MLIR, ordinary ScaIR, and dependent ScaIR
- representative row `128_128_12_64_*`
- `.input.mlir`, `.tiled.mlir`, `.guarded.mlir`, and `.llvm.mlir` where present

Decision: the saved row captures the supporting `K=K0*K1` reduction-dimension story: ordinary tail,
dependent guarded tail, dependent tail removal, and lowered executable form.

### `tiling_benchmarks/matmul_full_factorized_tiling_benchmark`

Saved files:

- source routes for MLIR, ordinary ScaIR, and dependent ScaIR
- representative row `2_64_2_64_12_64_*`
- `.input.mlir`, `.tiled.mlir`, `.guarded.mlir`, and `.llvm.mlir` where present

Decision: this is the composition benchmark. The saved row shows simultaneous `M/N/K` tiling, the
dependent guarded phase, the simplified exact phase, and lowered MLIR for comparison.

### `tiling_benchmarks/conv2d_output_dim_tiling_benchmark`

Saved files:

- source routes for MLIR, ordinary ScaIR, and dependent ScaIR
- representative default row `1_8_4_4_4_8_4_8_4_8_3_3_*`
- `.input.mlir`, `.tiled.mlir`, and `.guarded.mlir` where present

Decision: this is the fair Conv2D output-loop comparison. The saved files show the same
`n/cout/oh/ow` loop target across routes and the dependent guard-removal result.

### `tiling_benchmarks/conv2d_reduction_dim_tiling_benchmark`

Saved files:

- source routes for MLIR, ordinary ScaIR, and dependent ScaIR
- representative row `8_4_4_34_34_32_3_3_32_32_*`
- `.input.mlir`, `.tiled.mlir`, and `.guarded.mlir` where present

Decision: this benchmark is supporting reduction-domain evidence. The saved row shows tiling of
`Cin*Kh*Kw` by the intended `Cin1*Kh*Kw` factor and the dependent removal of reduction tails.

### `tiling_benchmarks/conv2d_full_factorized_tiling_benchmark`

Saved files:

- source routes for MLIR, ordinary ScaIR, and dependent ScaIR
- representative default row `1_8_4_4_4_8_4_8_4_8_3_3_*`
- `.input.mlir`, `.tiled.mlir`, and `.guarded.mlir` where present

Decision: this is the Conv2D composition benchmark. The saved files show output and reduction tiling
together, including the dependent guarded phase and the tail-free final phase.

## Structural Benchmarks

### `structural_benchmarks/strided_matmul_benchmark`

Saved files:

- MLIR, ScaIR baseline, and `d_memref` source kernels
- lowered `*.llvm.mlir` for all three routes

Decision: this benchmark's claim is descriptor lowering, so the representative artifacts are the
source kernels and lowered LLVM-dialect MLIR where descriptor extract/insert differences are
visible.

### `structural_benchmarks/convolution_benchmark`

Saved files:

- Conv2D MLIR, ScaIR baseline, and value-dependent source kernels
- lowered `*.llvm.mlir` for all three routes

Decision: the representative artifacts mirror the strided matmul benchmark: source kernels plus
lowered LLVM-dialect MLIR showing descriptor plumbing differences.

### `structural_benchmarks/semi_affine_indexing_benchmark`

Saved files:

- MLIR, ScaIR baseline, and value-dependent source kernels
- lowered `*.llvm.mlir` for all three routes

Decision: this benchmark demonstrates semi-affine dynamic indexing and lowering viability. Source
and lowered MLIR are the meaningful phases.
