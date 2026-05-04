# experiments

This directory contains the thesis-facing benchmark suite. Each benchmark writes
a family-local `out/metrics.csv`, and `build_all_metrics.sh` concatenates those
files when their shared 54-column header matches.

Metric interpretation:
- File metrics such as source bytes, source lines, lowered MLIR lines, and LLVM IR lines are measured from files on disk with `wc`.
- Structural IR metrics are produced from parsed IR with `scair-opt --emit-ir-metrics`, with an `mlir-opt` statistics fallback where needed.
- Runtime median and IQR are computed from repeated benchmark driver executions; raw samples are written beside each output file.
- `result`, `expected_result`, and `checksum` are emitted by benchmark drivers after route-specific correctness checks.
- `notes` contains benchmark-specific artifact checks, often implemented as regex checks over generated IR; these are supporting indicators and should be interpreted with the benchmark README.

Limitations:
- Supporting microbenchmarks demonstrate code-generation structure and validation, not broad application speedup claims.
- `scair_baseline` means ScaIR dynamic memref baseline lowering, not dependent `d_memref` / `dtensor.nat.mul` provenance.
- `MATMUL_TILING_PROFILE=cache_sweep` adds cache-sensitive explicit tile-size rows for the matmul tiling benchmark, but it is not an auto-tuning or hardware-optimality claim.
- Clean final thesis runs should regenerate `out/` directories from scratch so stale artifacts cannot be confused with current `metrics.csv` rows.
