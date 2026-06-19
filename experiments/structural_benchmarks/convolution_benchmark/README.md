# convolution_benchmark

$O_h = H-K_h+1,\qquad O_w = W-K_w+1$

$Y_{n,c_o,o_h,o_w} = \sum_{c_i=0}^{C_i-1} \sum_{k_h=0}^{K_h-1} \sum_{k_w=0}^{K_w-1} X_{n,c_i,o_h+k_h,o_w+k_w}\, K_{c_o,c_i,k_h,k_w}$

$\mathrm{checksum} = \sum_{n=0}^{N-1} \sum_{c_o=0}^{C_o-1} \sum_{o_h=0}^{O_h-1} \sum_{o_w=0}^{O_w-1} Y_{n,c_o,o_h,o_w}$

Tests:

- core descriptor-free lowering benchmark for selected Conv2D kernels
- compares upstream MLIR, ScaIR dynamic, and value-dependent/refined routes
- validates numerical output through the recorded checksum/result fields

Use in thesis:

- structural/code-generation evidence that selected value-dependent/refined layout routes can avoid
  unnecessary memref descriptor extract/insert plumbing
- runtime checksum validation for the generated routes
- large image-like cases may use fewer driver iterations per timing sample to keep final runs
  executable
- not a full memref ABI or bufferization result, and not a broad Conv2D speedup claim

Metric interpretation / limitations:

- The thesis-facing structural metric is the descriptor plumbing in lowered LLVM dialect, especially
  `llvm.extractvalue` and `llvm.insertvalue` counts.
- The refined route is expected to lower selected kernel-local views to direct pointer-plus-index
  arithmetic when sizes and strides are already available as scalar layout parameters.
- Runtime measurements validate execution and checksum; in current smoke data the structurally
  cleaner route is not uniformly the fastest route.

Run:

```bash
ITERATIONS=1 \
CONV_SIZE_SET=1x3x32x32x16x3x3 \
experiments/structural_benchmarks/convolution_benchmark/build_scair_example.sh
```

Useful environment variables:

- `CONV_SIZE_SET`: comma-separated `NxCixHxWxCoxKhxKw` entries
- `ITERATIONS`, `CONV_LARGE_ITERATIONS`
- `BENCH_WARMUP_REPS`, `BENCH_TIMING_REPS`
- `LLVM_BUILD_DIR`, `CC`, `OUT_DIR`

Outputs:

- `out/metrics.csv`
- `out/summary.md`
- route-local MLIR/LLVM artifacts, executables, output logs, and raw timings
