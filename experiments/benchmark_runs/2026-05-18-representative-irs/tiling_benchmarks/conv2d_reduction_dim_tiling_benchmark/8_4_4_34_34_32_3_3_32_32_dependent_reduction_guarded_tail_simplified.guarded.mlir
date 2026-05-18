#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @conv2d_reduction_dim_tiling(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.posnat, %3: !dtensor.nat, %4: !dtensor.nat, %5: !dtensor.nat, %6: !dtensor.posnat, %7: !dtensor.posnat, %8: !dtensor.nat, %9: !dtensor.nat, %10: !d_memref.memref<[], f32>, %11: !d_memref.memref<[], f32>, %12: !d_memref.memref<[], f32>) {
    %13 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %14 = "dtensor.nat.mul"(%6, %7) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
    %15 = "dtensor.nat.mul"(%2, %14) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
    %16 = "dtensor.nat.mul"(%1, %15) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %17 = "arith.constant"() <{value = 0 : index}> : () -> index
    %18 = "arith.constant"() <{value = 1 : index}> : () -> index
    %19 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %20 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
    %21 = "dtensor.shape.to_index"(%13) : (!dtensor.nat) -> index
    %22 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %23 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
    %24 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
    %25 = "dtensor.shape.to_index"(%7) : (!dtensor.posnat) -> index
    %26 = "dtensor.shape.to_index"(%8) : (!dtensor.nat) -> index
    %27 = "dtensor.shape.to_index"(%9) : (!dtensor.nat) -> index
    %28 = "dtensor.shape.to_index"(%14) : (!dtensor.posnat) -> index
    %29 = "dtensor.shape.to_index"(%16) : (!dtensor.nat) -> index
    %30 = "arith.muli"(%22, %23) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %31 = "arith.muli"(%21, %30) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %32 = "arith.muli"(%26, %27) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %33 = "arith.muli"(%24, %32) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %34 = d_memref.reinterpret_cast %10
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %13, %8, %9, %6, %7], f32, offset: 0, strides: [%31, %30, %23, %18, %23, %18]>
    %35 = d_memref.reinterpret_cast %11
    : !d_memref.memref<[], f32> to !d_memref.memref<[%5, %13, %6, %7], f32, offset: 0, strides: [%29, %28, %25, %18]>
    %36 = d_memref.reinterpret_cast %12
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %5, %8, %9], f32, offset: 0, strides: [%33, %32, %27, %18]>
    d_affine.for %37 = #map(%17) to #map(%20) step 1 : index {
      d_affine.for %38 = #map(%17) to #map(%24) step 1 : index {
        d_affine.for %39 = #map(%17) to #map(%26) step 1 : index {
          d_affine.for %40 = #map(%17) to #map(%27) step 1 : index {
            %41 = d_affine.for %42 = #map(%17) to #map(%29) step %25 : index iter_args(%43 = %19 : f32) {
              %44 = "arith.addi"(%42, %25) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
              %45 = "arith.minsi"(%44, %29) : (index, index) -> index
              %46 = d_affine.for %47 = #map(%42) to #map(%45) step 1 : i32 iter_args(%48 = %43 : f32) {
                %49 = "arith.divui"(%47, %28) : (index, index) -> index
                %50 = "arith.remui"(%47, %28) : (index, index) -> index
                %51 = "arith.divui"(%50, %25) : (index, index) -> index
                %52 = "arith.remui"(%50, %25) : (index, index) -> index
                %53 = d_memref.load %34[%37, %49, %39, %40, %51, %52] : !d_memref.memref<[%0, %13, %8, %9, %6, %7], f32, offset: 0, strides: [%31, %30, %23, %18, %23, %18]> -> f32
                %54 = d_memref.load %35[%38, %49, %51, %52] : !d_memref.memref<[%5, %13, %6, %7], f32, offset: 0, strides: [%29, %28, %25, %18]> -> f32
                %55 = "arith.mulf"(%53, %54) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                %56 = "arith.addf"(%48, %55) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                d_affine.yield %56 : (f32)
              }
              d_affine.yield %46 : (f32)
            }
            d_memref.store %41, %36[%37, %38, %39, %40] : f32, !d_memref.memref<[%0, %5, %8, %9], f32, offset: 0, strides: [%33, %32, %27, %18]>
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }
    func.return
  }
}
