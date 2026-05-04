# Broadcast Affine 2D Supporting Microbenchmark Summary

| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=3;k=12288 | ok | ok | 19 | 1 | 8 | 94 | 106 | 94.32 | 368575 | 368575 | 7346.38 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=3;k=12288 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1438.41 | 368575 | 368575 | 5819.93 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=3;k=12288 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1363.95 | 368575 | 368575 | 6176.09 |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=5;k=20480 | ok | ok | 19 | 1 | 8 | 94 | 106 | 98.54 | 814974 | 814974 | 13233.34 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=5;k=20480 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1400.90 | 814974 | 814974 | 11839.25 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=5;k=20480 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1412.00 | 814974 | 814974 | 11465.96 |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=7;k=28672 | ok | ok | 19 | 1 | 8 | 94 | 106 | 101.15 | 1044352 | 1044352 | 15689.10 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=7;k=28672 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1385.06 | 1044352 | 1044352 | 13451.65 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=7;k=28672 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1330.93 | 1044352 | 1044352 | 14359.69 |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=8;k=32768 | ok | ok | 19 | 1 | 8 | 94 | 106 | 97.60 | 1232761 | 1232761 | 16410.98 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=8;k=32768 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1421.61 | 1232761 | 1232761 | 14226.30 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=8;k=32768 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1338.80 | 1232761 | 1232761 | 14461.21 |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=16;k=65536 | ok | ok | 19 | 1 | 8 | 94 | 106 | 103.00 | 2588626 | 2588626 | 26576.82 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=16;k=65536 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1339.02 | 2588626 | 2588626 | 25956.04 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=16;k=65536 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1325.14 | 2588626 | 2588626 | 25605.65 |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=32;k=131072 | ok | ok | 19 | 1 | 8 | 94 | 106 | 99.55 | 5250974 | 5250974 | 51374.06 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=32;k=131072 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1349.83 | 5250974 | 5250974 | 50727.16 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=32;k=131072 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1308.60 | 5250974 | 5250974 | 50507.05 |
| broadcast_affine_2d | mlir_baseline | k0=4096;k1=64;k=262144 | ok | ok | 19 | 1 | 8 | 94 | 106 | 108.61 | 10661774 | 10661774 | 101771.15 |
| broadcast_affine_2d | scair_baseline | k0=4096;k1=64;k=262144 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1360.75 | 10661774 | 10661774 | 101245.99 |
| broadcast_affine_2d | value_dependent | k0=4096;k1=64;k=262144 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1535.75 | 10661774 | 10661774 | 101581.50 |
| broadcast_affine_2d | mlir_baseline | k0=16384;k1=8;k=131072 | ok | ok | 19 | 1 | 8 | 94 | 106 | 104.33 | 4931518 | 4931518 | 64514.76 |
| broadcast_affine_2d | scair_baseline | k0=16384;k1=8;k=131072 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1456.18 | 4931518 | 4931518 | 56751.46 |
| broadcast_affine_2d | value_dependent | k0=16384;k1=8;k=131072 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1335.83 | 4931518 | 4931518 | 57579.29 |
| broadcast_affine_2d | mlir_baseline | k0=16384;k1=16;k=262144 | ok | ok | 19 | 1 | 8 | 94 | 106 | 107.10 | 10354523 | 10354523 | 107344.17 |
| broadcast_affine_2d | scair_baseline | k0=16384;k1=16;k=262144 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1364.14 | 10354523 | 10354523 | 106593.51 |
| broadcast_affine_2d | value_dependent | k0=16384;k1=16;k=262144 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1374.71 | 10354523 | 10354523 | 110776.65 |
| broadcast_affine_2d | mlir_baseline | k0=16384;k1=32;k=524288 | ok | ok | 19 | 1 | 8 | 94 | 106 | 128.80 | 21004079 | 21004079 | 206873.24 |
| broadcast_affine_2d | scair_baseline | k0=16384;k1=32;k=524288 | ok | ok | 18 | 1 | 8 | 94 | 99 | 1366.67 | 21004079 | 21004079 | 203837.40 |
| broadcast_affine_2d | value_dependent | k0=16384;k1=32;k=524288 | ok | ok | 24 | 1 | 8 | 36 | 47 | 1340.01 | 21004079 | 21004079 | 202570.58 |

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
