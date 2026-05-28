# strided_matmul_benchmark

$C[i\,s^C_0+j\,s^C_1] = \sum_{p=0}^{K-1} A[i\,s^A_0+p\,s^A_1]\, B[p\,s^B_0+j\,s^B_1] \qquad 0\leq i<N,\;0\leq j<M$

Tests:

- core descriptor-free lowering benchmark for selected strided GEMM kernels
- compares upstream MLIR, ScaIR dynamic, and value-dependent/d_memref routes
- validates numerical output through the recorded checksum/result fields

Use in thesis:

- structural/code-generation evidence that selected refined `d_memref` layout routes can avoid
  unnecessary memref descriptor extract/insert plumbing
- runtime checksum validation for the generated routes
- not factorization-aware tiling evidence, not exact product tiling evidence, not a full memref ABI
  or bufferization result, and not a broad GEMM speedup claim

Metric interpretation / limitations:

- The thesis-facing structural metric is the descriptor plumbing in lowered LLVM dialect, especially
  `llvm.extractvalue` and `llvm.insertvalue` counts.
- The refined route is expected to lower selected kernel-local views to direct pointer-plus-index
  arithmetic when sizes and strides are already available as scalar layout parameters.
- Runtime measurements validate execution and checksum; they should not be used as a broad
  performance claim.

Run:

```bash
ITERATIONS=1 \
GEMM_SIZE_SET=128x128x128 \
experiments/structural_benchmarks/strided_matmul_benchmark/build_scair_example.sh
```

Useful environment variables:

- `GEMM_SIZE_SET`: comma-separated `NxMxK` entries
- `GEMM_INCLUDE_1024=1` to include the larger optional case
- `ITERATIONS`, `BENCH_WARMUP_REPS`, `BENCH_TIMING_REPS`
- `LLVM_BUILD_DIR`, `CC`, `OUT_DIR`

Outputs:

- `out/metrics.csv`
- `out/summary.md`
- route-local MLIR/LLVM artifacts, executables, output logs, and raw timings
