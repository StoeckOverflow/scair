# Convolution Benchmark Summary

| Benchmark     | Variant         | Rep                                                       | Build | Run | Structural ops | Func defs | Block args | MLIR LOC | LLVM LOC | Compile ms | Result       | Expected     |      ns/iter |
| ------------- | --------------- | --------------------------------------------------------- | ----- | --- | -------------: | --------: | ---------: | -------: | -------: | ---------: | ------------ | ------------ | -----------: |
| conv2d_kernel | mlir_baseline   | n=1;cin=3;h=32;w=32;cout=16;kh=3;kw=3;layout=NCHW/OIHW    | ok    | ok  |             34 |         1 |         22 |      213 |      240 |      22.52 | 388800.0     | 388800.0     |    395944.64 |
| conv2d_kernel | scair_baseline  | n=1;cin=3;h=32;w=32;cout=16;kh=3;kw=3;layout=NCHW/OIHW    | ok    | ok  |             34 |         1 |         22 |      176 |      199 |    1456.57 | 388800.0     | 388800.0     |    255178.32 |
| conv2d_kernel | value_dependent | n=1;cin=3;h=32;w=32;cout=16;kh=3;kw=3;layout=NCHW/OIHW    | ok    | ok  |             43 |         1 |         22 |       99 |      133 |    1479.22 | 388800.0     | 388800.0     |    258313.80 |
| conv2d_kernel | mlir_baseline   | n=1;cin=16;h=64;w=64;cout=32;kh=3;kw=3;layout=NCHW/OIHW   | ok    | ok  |             34 |         1 |         22 |      213 |      240 |      22.52 | 17713152.0   | 17713152.0   |  10563921.10 |
| conv2d_kernel | scair_baseline  | n=1;cin=16;h=64;w=64;cout=32;kh=3;kw=3;layout=NCHW/OIHW   | ok    | ok  |             34 |         1 |         22 |      176 |      199 |    1456.57 | 17713152.0   | 17713152.0   |  10345633.24 |
| conv2d_kernel | value_dependent | n=1;cin=16;h=64;w=64;cout=32;kh=3;kw=3;layout=NCHW/OIHW   | ok    | ok  |             43 |         1 |         22 |       99 |      133 |    1479.22 | 17713152.0   | 17713152.0   |  10322449.10 |
| conv2d_kernel | mlir_baseline   | n=1;cin=64;h=224;w=224;cout=64;kh=3;kw=3;layout=NCHW/OIHW | ok    | ok  |             34 |         1 |         22 |      213 |      240 |      22.52 | 1816805376.0 | 1816805376.0 | 942584334.00 |
| conv2d_kernel | scair_baseline  | n=1;cin=64;h=224;w=224;cout=64;kh=3;kw=3;layout=NCHW/OIHW | ok    | ok  |             34 |         1 |         22 |      176 |      199 |    1456.57 | 1816805376.0 | 1816805376.0 | 986056449.00 |
| conv2d_kernel | value_dependent | n=1;cin=64;h=224;w=224;cout=64;kh=3;kw=3;layout=NCHW/OIHW | ok    | ok  |             43 |         1 |         22 |       99 |      133 |    1479.22 | 1816805376.0 | 1816805376.0 | 952040405.00 |
