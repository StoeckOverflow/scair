# Blocked Pack Supporting Benchmark Summary

| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
| blocked_pack | mlir_baseline | mo=64;no=64;tm=16;tn=16;m=1024;n=1024;elements=1048576 | ok | ok | 33 | 1 | 10 | 92 | 114 | 113.65 | 17179344896 | 17179344896 | 389251.91 |
| blocked_pack | scair_baseline | mo=64;no=64;tm=16;tn=16;m=1024;n=1024;elements=1048576 | ok | ok | 19 | 1 | 10 | 107 | 120 | 1364.84 | 17179344896 | 17179344896 | 309961.30 |
| blocked_pack | value_dependent | mo=64;no=64;tm=16;tn=16;m=1024;n=1024;elements=1048576 | ok | ok | 31 | 1 | 10 | 60 | 79 | 1391.00 | 17179344896 | 17179344896 | 356226.65 |
| blocked_pack | mlir_baseline | mo=128;no=32;tm=8;tn=32;m=1024;n=1024;elements=1048576 | ok | ok | 33 | 1 | 10 | 92 | 114 | 112.31 | 17179344896 | 17179344896 | 293132.99 |
| blocked_pack | scair_baseline | mo=128;no=32;tm=8;tn=32;m=1024;n=1024;elements=1048576 | ok | ok | 19 | 1 | 10 | 107 | 120 | 1378.41 | 17179344896 | 17179344896 | 288225.61 |
| blocked_pack | value_dependent | mo=128;no=32;tm=8;tn=32;m=1024;n=1024;elements=1048576 | ok | ok | 31 | 1 | 10 | 60 | 79 | 1396.92 | 17179344896 | 17179344896 | 284920.36 |
| blocked_pack | mlir_baseline | mo=128;no=64;tm=16;tn=16;m=2048;n=1024;elements=2097152 | ok | ok | 33 | 1 | 10 | 92 | 114 | 103.51 | 34358689792 | 34358689792 | 778039.80 |
| blocked_pack | scair_baseline | mo=128;no=64;tm=16;tn=16;m=2048;n=1024;elements=2097152 | ok | ok | 19 | 1 | 10 | 107 | 120 | 1354.48 | 34358689792 | 34358689792 | 775832.82 |
| blocked_pack | value_dependent | mo=128;no=64;tm=16;tn=16;m=2048;n=1024;elements=2097152 | ok | ok | 31 | 1 | 10 | 60 | 79 | 1334.95 | 34358689792 | 34358689792 | 773673.14 |

## Metric Definitions

- `Benchmark`: benchmark or benchmark family member represented by the row.
- `Variant`: implementation route being compared, for example `mlir_baseline`, `scair_baseline`, `debruijn`, or `value_dependent`.
- `Rep`: representation-specific note for the row. For selector experiments this records the selector setting, such as `selector=0` or `selector=1`.
- `Build`: build outcome for the benchmark artifact. `ok` means the benchmark built successfully. `unsupported` means the pipeline failed or the route is not currently supported.
- `Run`: benchmark execution outcome. `ok` means the executable ran and produced timing/result data. `NA` means no run data was produced.
- `Structural ops`: total parsed IR operation nodes in the measured source IR. This is a parser-backed structural count, not a line count and not a regex/text estimate.
- `Func defs`: parsed count of function definition operations in the measured IR, currently `func.func` plus `llvm.func`.
- `Block args`: parsed count of SSA block arguments across all blocks in the measured IR.
- `MLIR LOC`: line count of the emitted lowered MLIR artifact on disk, measured with `wc -l`. This is a textual file metric taken after the MLIR file has been generated.
- `LLVM LOC`: line count of the emitted LLVM IR `.ll` artifact on disk, measured with `wc -l`. This is a textual file metric taken after the LLVM IR file has been generated.
- `Compile ms`: wall-clock build time for the benchmark pipeline, reported in milliseconds.
- `Result`: observed benchmark result value produced by the executable.
- `Expected`: expected benchmark result used as a correctness check.
- `ns/iter`: median runtime in nanoseconds per iteration across repeated benchmark runs.
- `parse_time_ms`, `verification_time_ms`, `lowering_time_ms`, `compile_total_ms`: compile-time split for routes that expose stage timing. `NA` means the split is not available yet for that family.
- `runtime_iqr_ns_per_iter`: interquartile range across the recorded repetitions.
- `checksum`: correctness guard value recorded by the benchmark driver when available.
- `checksum_status`: `ok`, `fail`, or `NA` depending on whether a checksum-based validation was emitted.
- `env_path`: captured environment snapshot for the benchmark family output directory.
- `raw_timings_path`: raw per-repetition timing samples in nanoseconds per iteration.
