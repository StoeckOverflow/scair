#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 32, s0)>
module {
  func.func private @bench_expf(f32) -> f32
  func.func private @bench_inv_sqrt_index(index) -> f32
  func.func @attention_mha(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: memref<?xf32>, %arg5: memref<?xf32>, %arg6: memref<?xf32>, %arg7: memref<?xf32>, %arg8: memref<?xf32>, %arg9: memref<?xf32>, %arg10: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %cst_0 = arith.constant -3.40282347E+38 : f32
    %0 = arith.muli %arg2, %arg3 : index
    %1 = arith.muli %arg1, %0 : index
    %2 = arith.muli %arg1, %arg1 : index
    %3 = arith.muli %arg2, %2 : index
    %4 = arith.muli %arg3, %2 : index
    %reinterpret_cast = memref.reinterpret_cast %arg4 to offset: [%c0], sizes: [%arg0, %arg1, %0], strides: [%1, %0, %c1] : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg5 to offset: [%c0], sizes: [%arg0, %arg1, %0], strides: [%1, %0, %c1] : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
    %reinterpret_cast_2 = memref.reinterpret_cast %arg6 to offset: [%c0], sizes: [%arg0, %arg1, %0], strides: [%1, %0, %c1] : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
    %reinterpret_cast_3 = memref.reinterpret_cast %arg7 to offset: [%c0], sizes: [%arg0, %arg2, %arg1, %arg1], strides: [%3, %2, %arg1, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_4 = memref.reinterpret_cast %arg8 to offset: [%c0], sizes: [%arg0, %arg2, %arg1, %arg1], strides: [%3, %2, %arg1, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_5 = memref.reinterpret_cast %arg10 to offset: [%c0], sizes: [%arg0, %arg1, %0], strides: [%1, %0, %c1] : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
    %reinterpret_cast_6 = memref.reinterpret_cast %arg9 to offset: [%c0], sizes: [%arg0, %arg1, %0], strides: [%1, %0, %c1] : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
    %5 = call @bench_inv_sqrt_index(%arg3) : (index) -> f32
    affine.for %arg11 = 0 to %arg0 step 32 {
      affine.for %arg12 = 0 to %arg2 step 32 {
        affine.for %arg13 = #map(%arg11) to min #map1(%arg11)[%arg0] {
          affine.for %arg14 = #map(%arg12) to min #map1(%arg12)[%arg2] {
            %6 = arith.muli %arg14, %arg3 : index
            affine.for %arg15 = 0 to %arg1 {
              affine.for %arg16 = 0 to %arg1 {
                %7 = affine.for %arg17 = 0 to %arg3 iter_args(%arg18 = %cst) -> (f32) {
                  %9 = arith.addi %6, %arg17 : index
                  %10 = memref.load %reinterpret_cast[%arg13, %arg15, %9] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
                  %11 = memref.load %reinterpret_cast_1[%arg13, %arg16, %9] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
                  %12 = arith.mulf %10, %11 : f32
                  %13 = arith.addf %arg18, %12 : f32
                  affine.yield %13 : f32
                }
                %8 = arith.mulf %7, %5 : f32
                memref.store %8, %reinterpret_cast_3[%arg13, %arg14, %arg15, %arg16] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
              }
            }
          }
        }
      }
    }
    affine.for %arg11 = 0 to %arg0 step 32 {
      affine.for %arg12 = 0 to %arg2 step 32 {
        affine.for %arg13 = 0 to %arg1 step 32 {
          affine.for %arg14 = #map(%arg11) to min #map1(%arg11)[%arg0] {
            affine.for %arg15 = #map(%arg12) to min #map1(%arg12)[%arg2] {
              affine.for %arg16 = #map(%arg13) to min #map1(%arg13)[%arg1] {
                %6 = affine.for %arg17 = 0 to %arg1 iter_args(%arg18 = %cst_0) -> (f32) {
                  %8 = memref.load %reinterpret_cast_3[%arg14, %arg15, %arg16, %arg17] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                  %9 = arith.maximumf %arg18, %8 : f32
                  affine.yield %9 : f32
                }
                %7 = affine.for %arg17 = 0 to %arg1 iter_args(%arg18 = %cst) -> (f32) {
                  %8 = memref.load %reinterpret_cast_3[%arg14, %arg15, %arg16, %arg17] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                  %9 = arith.subf %8, %6 : f32
                  %10 = func.call @bench_expf(%9) : (f32) -> f32
                  memref.store %10, %reinterpret_cast_4[%arg14, %arg15, %arg16, %arg17] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                  %11 = arith.addf %arg18, %10 : f32
                  affine.yield %11 : f32
                }
                affine.for %arg17 = 0 to %arg1 {
                  %8 = memref.load %reinterpret_cast_4[%arg14, %arg15, %arg16, %arg17] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                  %9 = arith.divf %8, %7 : f32
                  memref.store %9, %reinterpret_cast_4[%arg14, %arg15, %arg16, %arg17] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                }
              }
            }
          }
        }
      }
    }
    affine.for %arg11 = 0 to %arg0 step 32 {
      affine.for %arg12 = 0 to %arg1 step 32 {
        affine.for %arg13 = 0 to %arg2 step 32 {
          affine.for %arg14 = #map(%arg11) to min #map1(%arg11)[%arg0] {
            affine.for %arg15 = #map(%arg12) to min #map1(%arg12)[%arg1] {
              affine.for %arg16 = #map(%arg13) to min #map1(%arg13)[%arg2] {
                %6 = arith.muli %arg16, %arg3 : index
                affine.for %arg17 = 0 to %arg3 {
                  %7 = arith.addi %6, %arg17 : index
                  %8 = affine.for %arg18 = 0 to %arg1 iter_args(%arg19 = %cst) -> (f32) {
                    %9 = memref.load %reinterpret_cast_4[%arg14, %arg16, %arg15, %arg18] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                    %10 = memref.load %reinterpret_cast_2[%arg14, %arg18, %7] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
                    %11 = arith.mulf %9, %10 : f32
                    %12 = arith.addf %arg19, %11 : f32
                    affine.yield %12 : f32
                  }
                  memref.store %8, %reinterpret_cast_6[%arg14, %arg15, %7] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
                }
              }
            }
          }
        }
      }
    }
    affine.for %arg11 = 0 to %arg0 {
      affine.for %arg12 = 0 to %arg1 {
        %6 = affine.for %arg13 = 0 to %0 iter_args(%arg14 = %cst) -> (f32) {
          %7 = memref.load %reinterpret_cast_6[%arg11, %arg12, %arg13] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
          memref.store %7, %reinterpret_cast_5[%arg11, %arg12, %arg13] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
          affine.yield %7 : f32
        }
      }
    }
    return
  }
}

