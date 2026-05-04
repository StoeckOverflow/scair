# Strided Matmul Benchmark Summary

| Benchmark | Variant | Rep | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result | Expected | ns/iter |
| --- | --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- | ---: |
| matmul_strided | mlir_baseline | n=128;m=128;k=128 | ok | ok | 18 | 1 | 16 | 132 | 138 | 17.9 | 128.0 | 128.0 | 602313.10 |
| matmul_strided | scair_baseline | n=128;m=128;k=128 | ok | ok | 17 | 1 | 16 | 93 | 103 | 1218.24 | 128.0 | 128.0 | 605208.07 |
| matmul_strided | scair_dmemref | n=128;m=128;k=128 | ok | ok | 22 | 1 | 16 | 56 | 70 | 1226.8 | 128.0 | 128.0 | 605897.75 |
| matmul_strided | mlir_baseline | n=256;m=256;k=256 | ok | ok | 18 | 1 | 16 | 132 | 138 | 17.9 | 256.0 | 256.0 | 5288918.93 |
| matmul_strided | scair_baseline | n=256;m=256;k=256 | ok | ok | 17 | 1 | 16 | 93 | 103 | 1218.24 | 256.0 | 256.0 | 5521691.80 |
| matmul_strided | scair_dmemref | n=256;m=256;k=256 | ok | ok | 22 | 1 | 16 | 56 | 70 | 1226.8 | 256.0 | 256.0 | 5459701.49 |
| matmul_strided | mlir_baseline | n=512;m=512;k=512 | ok | ok | 18 | 1 | 16 | 132 | 138 | 17.9 | 512.0 | 512.0 | 52038960.76 |
| matmul_strided | scair_baseline | n=512;m=512;k=512 | ok | ok | 17 | 1 | 16 | 93 | 103 | 1218.24 | 512.0 | 512.0 | 52783666.77 |
| matmul_strided | scair_dmemref | n=512;m=512;k=512 | ok | ok | 22 | 1 | 16 | 56 | 70 | 1226.8 | 512.0 | 512.0 | 53795376.59 |
