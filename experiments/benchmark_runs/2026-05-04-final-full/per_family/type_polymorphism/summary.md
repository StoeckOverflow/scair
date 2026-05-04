# Type Polymorphism Design Benchmark Summary

| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
| shared_polymorphic_identity_multitype | debruijn |  | ok | ok | 29 | 1 | 7 | 33 | 44 | 689.66 | 29 | 29 | 1.54 |
| shared_polymorphic_identity_multitype | value_dependent |  | ok | ok | 29 | 1 | 8 | 33 | 44 | 722.33 | 29 | 29 | 1.54 |
| shared_polymorphic_identity_multitype | mlir_baseline |  | ok | ok | 31 | 7 | 12 | 28 | 50 | 71.70 | 29 | 29 | 1.54 |
| shared_polymorphic_kernel_bank_multitype | debruijn |  | ok | ok | 280 | 1 | 22 | 374 | 427 | 1144.71 | 3090 | 3090 | 7.78 |
| shared_polymorphic_kernel_bank_multitype | value_dependent |  | ok | ok | 280 | 1 | 38 | 374 | 427 | 1122.81 | 3090 | 3090 | 7.78 |
| shared_polymorphic_kernel_bank_multitype | mlir_baseline |  | ok | ok | 324 | 49 | 54 | 427 | 427 | 102.44 | 3090 | 3090 | 7.72 |
