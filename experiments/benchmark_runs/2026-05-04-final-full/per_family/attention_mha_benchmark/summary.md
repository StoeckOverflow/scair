# Attention MHA Benchmark Summary

| Benchmark     | Variant         | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result     | Expected   |    ns/iter |
| ------------- | --------------- | --- | ----- | --- | -------------: | --------: | ---------: | -------: | -------: | ---------: | ---------- | ---------- | ---------: |
| attention_mha | mlir_baseline   |     | ok    | ok  |             87 |         3 |         35 |      630 |      738 |     151.27 | 58253.5117 | 58253.5117 | 9616581.63 |
| attention_mha | scair_baseline  |     | ok    | ok  |             66 |         3 |         33 |      476 |      534 |    1834.31 | 58253.5117 | 58253.5117 | 9793206.65 |
| attention_mha | value_dependent |     | ok    | ok  |             87 |         3 |         33 |      269 |      355 |    1793.90 | 58253.5117 | 58253.5117 | 9727580.92 |

## Metric Definitions

- `Benchmark`: benchmark or benchmark family member represented by the row.
- `Variant`: implementation route being compared, for example `mlir_baseline`, `scair_baseline`,
  `debruijn`, or `value_dependent`.
- `Rep`: representation-specific note for the row. For selector experiments this records the
  selector setting, such as `selector=0` or `selector=1`.
- `Build`: build outcome for the benchmark artifact. `ok` means the benchmark built successfully.
  `unsupported` means the pipeline failed or the route is not currently supported.
- `Run`: benchmark execution outcome. `ok` means the executable ran and produced timing/result data.
  `NA` means no run data was produced.
- `Structural ops`: total parsed IR operation nodes in the measured source IR. This is a
  parser-backed structural count, not a line count and not a regex/text estimate.
- `Func defs`: parsed count of function definition operations in the measured IR, currently
  `func.func` plus `llvm.func`.
- `Block args`: parsed count of SSA block arguments across all blocks in the measured IR.
- `MLIR LOC`: line count of the emitted lowered MLIR artifact on disk, measured with `wc -l`. This
  is a textual file metric taken after the MLIR file has been generated.
- `LLVM LOC`: line count of the emitted LLVM IR `.ll` artifact on disk, measured with `wc -l`. This
  is a textual file metric taken after the LLVM IR file has been generated.
- `Compile ms`: wall-clock build time for the benchmark pipeline, reported in milliseconds.
- `Result`: observed benchmark result value produced by the executable.
- `Expected`: expected benchmark result used as a correctness check.
- `ns/iter`: median runtime in nanoseconds per iteration across repeated benchmark runs.
- `parse_time_ms`, `verification_time_ms`, `lowering_time_ms`, `compile_total_ms`: compile-time
  split for routes that expose stage timing. `NA` means the split is not available yet for that
  family.
- `runtime_iqr_ns_per_iter`: interquartile range across the recorded repetitions.
- `checksum`: correctness guard value recorded by the benchmark driver when available.
- `checksum_status`: `ok`, `fail`, or `NA` depending on whether a checksum-based validation was
  emitted.
- `env_path`: captured environment snapshot for the benchmark family output directory.
- `raw_timings_path`: raw per-repetition timing samples in nanoseconds per iteration.
