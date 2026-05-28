# Matmul Factorization-Aware Tiling Benchmark Summary

| Benchmark     | Variant         | Rep                                                            | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result     | Expected   |    ns/iter |
| ------------- | --------------- | -------------------------------------------------------------- | ----- | --- | -------------: | --------: | ---------: | -------: | -------: | ---------: | ---------- | ---------- | ---------: |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=8                       | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     108.54 | 147453.344 | 147453.344 |  327127.33 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=16                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     127.49 | 147453.344 | 147453.344 |  341827.28 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=32                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     111.53 | 147453.344 | 147453.344 |  334058.06 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=64                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     109.14 | 147453.344 | 147453.344 |  337104.53 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=128                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     112.25 | 147453.344 | 147453.344 |  332244.20 |
| matmul_tiling | scair_baseline  | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=untiled                 | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1395.63 | 147453.344 | 147453.344 |  330462.09 |
| matmul_tiling | value_dependent | m=8;n=8;k0=4096;k1=3;k=12288;tile_size=dependent_factorized    | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1446.46 | 147453.344 | 147453.344 |  361120.72 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=8                       | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     121.09 | 245756.219 | 245756.219 |  541304.28 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=16                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     108.54 | 245756.219 | 245756.219 |  537199.23 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=32                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     108.40 | 245756.219 | 245756.219 |  536519.10 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=64                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     113.28 | 245756.219 | 245756.219 |  542725.99 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=128                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     111.16 | 245756.219 | 245756.219 |  544360.60 |
| matmul_tiling | scair_baseline  | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=untiled                 | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1362.02 | 245756.219 | 245756.219 |  539639.50 |
| matmul_tiling | value_dependent | m=8;n=8;k0=4096;k1=5;k=20480;tile_size=dependent_factorized    | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1490.00 | 245756.219 | 245756.219 |  572070.29 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=8                       | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     107.66 | 344062.062 | 344062.062 |  762499.69 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=16                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     107.94 | 344062.062 | 344062.062 |  775875.16 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=32                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     108.37 | 344062.062 | 344062.062 |  772879.44 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=64                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     119.37 | 344062.062 | 344062.062 |  792887.93 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=128                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     111.53 | 344062.062 | 344062.062 |  780308.41 |
| matmul_tiling | scair_baseline  | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=untiled                 | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1375.86 | 344062.062 | 344062.062 |  774843.57 |
| matmul_tiling | value_dependent | m=8;n=8;k0=4096;k1=7;k=28672;tile_size=dependent_factorized    | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1511.06 | 344062.062 | 344062.062 |  827452.47 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=8                       | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     108.14 | 393212.938 | 393212.938 |  872415.35 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=16                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     107.66 | 393212.938 | 393212.938 |  872836.38 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=32                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     107.68 | 393212.938 | 393212.938 |  877493.33 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=64                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     128.41 | 393212.938 | 393212.938 |  916639.11 |
| matmul_tiling | mlir_baseline   | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=128                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     123.86 | 393212.938 | 393212.938 |  902589.47 |
| matmul_tiling | scair_baseline  | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=untiled                 | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1781.29 | 393212.938 | 393212.938 |  889669.34 |
| matmul_tiling | value_dependent | m=8;n=8;k0=4096;k1=8;k=32768;tile_size=dependent_factorized    | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1478.00 | 393212.938 | 393212.938 |  899624.37 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=8                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     115.61 | 294907.312 | 294907.312 |  682826.38 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=16                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     107.86 | 294907.312 | 294907.312 |  686768.96 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=32                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     109.73 | 294907.312 | 294907.312 |  674806.52 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=64                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     110.46 | 294907.312 | 294907.312 |  677519.03 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=128                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     114.28 | 294907.312 | 294907.312 |  698659.74 |
| matmul_tiling | scair_baseline  | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=untiled                | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1374.76 | 294907.312 | 294907.312 |  679661.71 |
| matmul_tiling | value_dependent | m=16;n=16;k0=2048;k1=3;k=6144;tile_size=dependent_factorized   | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1518.60 | 294907.312 | 294907.312 |  743793.17 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=8                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     107.34 | 491512.625 | 491512.625 | 1164829.10 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=16                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      98.90 | 491512.625 | 491512.625 | 1152340.56 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=32                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.21 | 491512.625 | 491512.625 | 1156771.11 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=64                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.14 | 491512.625 | 491512.625 | 1138390.12 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=128                   | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.63 | 491512.625 | 491512.625 | 1127534.22 |
| matmul_tiling | scair_baseline  | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=untiled               | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1356.21 | 491512.625 | 491512.625 | 1120021.04 |
| matmul_tiling | value_dependent | m=16;n=16;k0=2048;k1=5;k=10240;tile_size=dependent_factorized  | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1418.74 | 491512.625 | 491512.625 | 1146054.30 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=8                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      99.60 | 688126.375 | 688126.375 | 1570681.47 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=16                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      96.99 | 688126.375 | 688126.375 | 1579166.98 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=32                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      98.83 | 688126.375 | 688126.375 | 1565176.57 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=64                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.49 | 688126.375 | 688126.375 | 1557151.90 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=128                   | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      96.93 | 688126.375 | 688126.375 | 1556749.24 |
| matmul_tiling | scair_baseline  | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=untiled               | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1748.17 | 688126.375 | 688126.375 | 1562310.56 |
| matmul_tiling | value_dependent | m=16;n=16;k0=2048;k1=7;k=14336;tile_size=dependent_factorized  | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1412.88 | 688126.375 | 688126.375 | 1556767.92 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=8                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.33 | 786426.312 | 786426.312 | 1756530.93 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=16                   | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      95.53 | 786426.312 | 786426.312 | 1781279.33 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=32                   | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.59 | 786426.312 | 786426.312 | 1762728.59 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=64                   | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     109.33 | 786426.312 | 786426.312 | 1780345.55 |
| matmul_tiling | mlir_baseline   | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=128                  | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      95.68 | 786426.312 | 786426.312 | 1786736.91 |
| matmul_tiling | scair_baseline  | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=untiled              | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1353.52 | 786426.312 | 786426.312 | 1775988.55 |
| matmul_tiling | value_dependent | m=16;n=16;k0=1024;k1=16;k=16384;tile_size=dependent_factorized | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1433.83 | 786426.312 | 786426.312 | 1844954.17 |
| matmul_tiling | mlir_baseline   | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=8                      | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.22 | 786425.438 | 786425.438 | 1761430.74 |
| matmul_tiling | mlir_baseline   | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=16                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      97.60 | 786425.438 | 786425.438 | 1775456.39 |
| matmul_tiling | mlir_baseline   | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=32                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     103.58 | 786425.438 | 786425.438 | 1772887.31 |
| matmul_tiling | mlir_baseline   | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=64                     | ok    | ok  |             21 |         1 |         11 |      157 |      178 |     129.99 | 786425.438 | 786425.438 | 1788327.43 |
| matmul_tiling | mlir_baseline   | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=128                    | ok    | ok  |             21 |         1 |         11 |      157 |      178 |      96.01 | 786425.438 | 786425.438 | 1762033.15 |
| matmul_tiling | scair_baseline  | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=untiled                | ok    | ok  |             19 |         1 |         11 |       95 |      104 |    1380.49 | 786425.438 | 786425.438 | 1821071.73 |
| matmul_tiling | value_dependent | m=32;n=16;k0=1024;k1=8;k=8192;tile_size=dependent_factorized   | ok    | ok  |             24 |         1 |         11 |       58 |       78 |    1547.60 | 786425.438 | 786425.438 | 1835787.48 |

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
