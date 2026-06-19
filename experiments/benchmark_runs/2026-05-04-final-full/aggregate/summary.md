# Uniform Experiment Metrics Summary

## strided_matmul_benchmark

| Benchmark | Variant | Kernel | Size | Reps | Build | Run | Verify ms | Lowering pipeline ms | Staged total ms | Timed median ns/iter | Timed IQR ns/iter | Checksum | Checksum status | Commit | Env |
| --- | --- | --- | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | --- | --- |
| matmul_strided | mlir_baseline | gemm | n=128;m=128;k=128 | 15 | ok | ok | 8.99 | 8.89 | 17.9 | 602313.10 | 2910.62 | 128.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | mlir_baseline | gemm | n=256;m=256;k=256 | 15 | ok | ok | 8.99 | 8.89 | 17.9 | 5288918.93 | 128520.92 | 256.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | mlir_baseline | gemm | n=512;m=512;k=512 | 15 | ok | ok | 8.99 | 8.89 | 17.9 | 52038960.76 | 904975.83 | 512.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | scair_baseline | gemm | n=128;m=128;k=128 | 15 | ok | ok | 534.27 | 683.94 | 1218.24 | 605208.07 | 3325.10 | 128.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | scair_baseline | gemm | n=256;m=256;k=256 | 15 | ok | ok | 534.27 | 683.94 | 1218.24 | 5521691.80 | 41015.77 | 256.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | scair_baseline | gemm | n=512;m=512;k=512 | 15 | ok | ok | 534.27 | 683.94 | 1218.24 | 52783666.77 | 1168912.62 | 512.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | scair_dmemref | gemm | n=128;m=128;k=128 | 15 | ok | ok | 534.95 | 691.83 | 1226.8 | 605897.75 | 1238.41 | 128.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | scair_dmemref | gemm | n=256;m=256;k=256 | 15 | ok | ok | 534.95 | 691.83 | 1226.8 | 5459701.49 | 99458.83 | 256.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |
| matmul_strided | scair_dmemref | gemm | n=512;m=512;k=512 | 15 | ok | ok | 534.95 | 691.83 | 1226.8 | 53795376.59 | 327991.75 | 512.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/strided_matmul_benchmark/out/env.json |

## convolution_benchmark

| Benchmark | Variant | Kernel | Size | Reps | Build | Run | Verify ms | Lowering pipeline ms | Staged total ms | Timed median ns/iter | Timed IQR ns/iter | Checksum | Checksum status | Commit | Env |
| --- | --- | --- | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | --- | --- |
| conv2d_kernel | mlir_baseline | conv2d | n=1;cin=3;h=32;w=32;cout=16;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 10.73 | 11.76 | 22.52 | 395944.64 | 27091.68 | 388800.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | mlir_baseline | conv2d | n=1;cin=16;h=64;w=64;cout=32;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 10.73 | 11.76 | 22.52 | 10563921.10 | 86981.22 | 17713152.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | mlir_baseline | conv2d | n=1;cin=64;h=224;w=224;cout=64;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 10.73 | 11.76 | 22.52 | 942584334.00 | 17368173.00 | 1816805376.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | scair_baseline | conv2d | n=1;cin=3;h=32;w=32;cout=16;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 622.39 | 834.16 | 1456.57 | 255178.32 | 5915.60 | 388800.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | scair_baseline | conv2d | n=1;cin=16;h=64;w=64;cout=32;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 622.39 | 834.16 | 1456.57 | 10345633.24 | 124601.38 | 17713152.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | scair_baseline | conv2d | n=1;cin=64;h=224;w=224;cout=64;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 622.39 | 834.16 | 1456.57 | 986056449.00 | 24012722.00 | 1816805376.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | value_dependent | conv2d | n=1;cin=3;h=32;w=32;cout=16;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 622.06 | 857.13 | 1479.22 | 258313.80 | 2435.18 | 388800.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | value_dependent | conv2d | n=1;cin=16;h=64;w=64;cout=32;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 622.06 | 857.13 | 1479.22 | 10322449.10 | 95672.56 | 17713152.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |
| conv2d_kernel | value_dependent | conv2d | n=1;cin=64;h=224;w=224;cout=64;kh=3;kw=3;layout=NCHW/OIHW | 15 | ok | ok | 622.06 | 857.13 | 1479.22 | 952040405.00 | 14501885.00 | 1816805376.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/convolution_benchmark/out/env.json |

## semi_affine_indexing_benchmark

| Benchmark | Variant | Kernel | Size | Reps | Build | Run | Verify ms | Lowering pipeline ms | Staged total ms | Timed median ns/iter | Timed IQR ns/iter | Checksum | Checksum status | Commit | Env |
| --- | --- | --- | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | --- | --- |
| semi_affine_fill_and_sum | mlir_baseline | semi_affine | m=256;n=1024;layout=semi_affine | 15 | ok | ok | NA | NA | 33.18 | 133431.79 | 779.13 | 262144.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/semi_affine_indexing_benchmark/out/env.json |
| semi_affine_fill_and_sum | scair_baseline | semi_affine | m=256;n=1024;layout=semi_affine | 15 | ok | ok | NA | NA | 708.44 | 132844.48 | 322.38 | 262144.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/semi_affine_indexing_benchmark/out/env.json |
| semi_affine_fill_and_sum | value_dependent | semi_affine | m=256;n=1024;layout=semi_affine | 15 | ok | ok | NA | NA | 737.57 | 132963.91 | 347.37 | 262144.0 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/semi_affine_indexing_benchmark/out/env.json |

## type_polymorphism

| Benchmark | Variant | Kernel | Size | Reps | Build | Run | Verify ms | Lowering pipeline ms | Staged total ms | Timed median ns/iter | Timed IQR ns/iter | Checksum | Checksum status | Commit | Env |
| --- | --- | --- | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | --- | --- | --- | --- |
| shared_polymorphic_identity_multitype | mlir_baseline | type_polymorphism | benchmark=shared_polymorphic_identity_multitype;polymorphism=type_level | 15 | ok | ok | NA | NA | 71.70 | 1.54 | 0.01 | 29 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/type_polymorphism/out/env.json |
| shared_polymorphic_identity_multitype | debruijn | type_polymorphism | benchmark=shared_polymorphic_identity_multitype;polymorphism=type_level | 15 | ok | ok | NA | NA | 689.66 | 1.54 | 0.00 | 29 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/type_polymorphism/out/env.json |
| shared_polymorphic_identity_multitype | value_dependent | type_polymorphism | benchmark=shared_polymorphic_identity_multitype;polymorphism=type_level | 15 | ok | ok | NA | NA | 722.33 | 1.54 | 0.00 | 29 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/type_polymorphism/out/env.json |
| shared_polymorphic_kernel_bank_multitype | mlir_baseline | type_polymorphism | benchmark=shared_polymorphic_kernel_bank_multitype;polymorphism=type_level | 15 | ok | ok | NA | NA | 102.44 | 7.72 | 0.07 | 3090 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/type_polymorphism/out/env.json |
| shared_polymorphic_kernel_bank_multitype | debruijn | type_polymorphism | benchmark=shared_polymorphic_kernel_bank_multitype;polymorphism=type_level | 15 | ok | ok | NA | NA | 1144.71 | 7.78 | 0.06 | 3090 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/type_polymorphism/out/env.json |
| shared_polymorphic_kernel_bank_multitype | value_dependent | type_polymorphism | benchmark=shared_polymorphic_kernel_bank_multitype;polymorphism=type_level | 15 | ok | ok | NA | NA | 1122.81 | 7.78 | 0.08 | 3090 | ok | b3d15950d589ba2f2f5ee880cf5f459595ebd061 | /home/dominic/dev/scair/experiments/type_polymorphism/out/env.json |

## Notes

- `Timed median ns/iter` and `Timed IQR ns/iter` describe the benchmark's timed region, which may include required output reset/zeroing in addition to kernel execution.
- `verification_time_ms`, `lowering_time_ms`, and `compile_total_ms` are the thesis-facing staged tool timings for upgraded families.
- `lowering_time_ms` is an inclusive pipeline run over the source IR, not an isolated pass-only timer.
- `compile_total_ms` is the total staged tool time captured by `run_pipeline.py`, not full native-code build time.
- `compile_total_ms` falls back to legacy `compile_ms` when a family has not yet been upgraded to split compile timing.
- `runtime_median_ns_per_iter` falls back to legacy `runtime_ns_per_iter` for older rows.
- Weak structural-only families may still emit `NA` for checksum and compile sub-stages.
- `Commit` and `Env` identify the source revision and captured machine metadata for each row.
