# Semi-Affine Indexing Benchmark Summary

| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
| semi_affine_fill_and_sum | mlir_baseline |  | ok | ok | 21 | 1 | 10 | 105 | 116 | 33.18 | 262144.0 | 262144.0 | 133431.79 |
| semi_affine_fill_and_sum | scair_baseline |  | ok | ok | 21 | 1 | 10 | 80 | 93 | 708.44 | 262144.0 | 262144.0 | 132844.48 |
| semi_affine_fill_and_sum | value_dependent |  | ok | ok | 20 | 1 | 10 | 59 | 76 | 737.57 | 262144.0 | 262144.0 | 132963.91 |
