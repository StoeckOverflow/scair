#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0) -> (d0 + 32)>
#map2 = affine_map<(d0)[s0] -> (d0 + 32, s0)>
#set = affine_set<(d0, d1)[s0, s1] : (-d0 + s0 - 32 >= 0, -d1 + s1 - 32 >= 0)>
module {
  func.func @matmul_dynamic(%arg0: index, %arg1: index, %arg2: index, %arg3: memref<?x?xf32>, %arg4: memref<?x?xf32>, %arg5: memref<?x?xf32>) attributes {llvm.emit_c_interface} {
    %cst = arith.constant 0.000000e+00 : f32
    affine.for %arg6 = 0 to %arg0 step 32 {
      affine.for %arg7 = 0 to %arg1 step 32 {
        affine.if #set(%arg6, %arg7)[%arg0, %arg1] {
          affine.for %arg8 = #map(%arg6) to #map1(%arg6) {
            affine.for %arg9 = #map(%arg7) to #map1(%arg7) {
              %0 = affine.for %arg10 = 0 to %arg2 iter_args(%arg11 = %cst) -> (f32) {
                %1 = affine.load %arg3[%arg8, %arg10] : memref<?x?xf32>
                %2 = affine.load %arg4[%arg10, %arg9] : memref<?x?xf32>
                %3 = arith.mulf %1, %2 : f32
                %4 = arith.addf %arg11, %3 : f32
                affine.yield %4 : f32
              }
              affine.store %0, %arg5[%arg8, %arg9] : memref<?x?xf32>
            }
          }
        } else {
          affine.for %arg8 = #map(%arg6) to min #map2(%arg6)[%arg0] {
            affine.for %arg9 = #map(%arg7) to min #map2(%arg7)[%arg1] {
              %0 = affine.for %arg10 = 0 to %arg2 iter_args(%arg11 = %cst) -> (f32) {
                %1 = affine.load %arg3[%arg8, %arg10] : memref<?x?xf32>
                %2 = affine.load %arg4[%arg10, %arg9] : memref<?x?xf32>
                %3 = arith.mulf %1, %2 : f32
                %4 = arith.addf %arg11, %3 : f32
                affine.yield %4 : f32
              }
              affine.store %0, %arg5[%arg8, %arg9] : memref<?x?xf32>
            }
          }
        }
      }
    }
    return
  }
}

