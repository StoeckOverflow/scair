#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @matmul_outer_dim_tiling(%0: !dtensor.nat, %1: !dtensor.posnat, %2: !dtensor.nat, %3: !dtensor.posnat, %4: !dtensor.nat, %5: !d_memref.memref<[], f32>, %6: !d_memref.memref<[], f32>, %7: !d_memref.memref<[], f32>) attributes {scair.emit_bare_interface = true} {
    %8 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %9 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %10 = "arith.constant"() <{value = 0 : index}> : () -> index
    %11 = "arith.constant"() <{value = 1 : index}> : () -> index
    %12 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %13 = "dtensor.shape.to_index"(%8) : (!dtensor.nat) -> index
    %14 = "dtensor.shape.to_index"(%9) : (!dtensor.nat) -> index
    %15 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
    %16 = d_memref.reinterpret_cast %5
    : !d_memref.memref<[], f32> to !d_memref.memref<[%8, %4], f32, offset: 0, strides: [%15, %11]>
    %17 = d_memref.reinterpret_cast %6
    : !d_memref.memref<[], f32> to !d_memref.memref<[%4, %9], f32, offset: 0, strides: [%14, %11]>
    %18 = d_memref.reinterpret_cast %7
    : !d_memref.memref<[], f32> to !d_memref.memref<[%8, %9], f32, offset: 0, strides: [%14, %11]>
    %19 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
    d_affine.for %20 = #map(%10) to #map(%13) step %19 : index {
      %21 = "arith.addi"(%20, %19) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
      %22 = "arith.minsi"(%21, %13) : (index, index) -> index
      d_affine.for %23 = #map(%20) to #map(%22) step 1 : index {
        %24 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
        d_affine.for %25 = #map(%10) to #map(%14) step %24 : index {
          %26 = "arith.addi"(%25, %24) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          %27 = "arith.minsi"(%26, %14) : (index, index) -> index
          d_affine.for %28 = #map(%25) to #map(%27) step 1 : index {
            %29 = d_affine.for %30 = #map(%10) to #map(%15) step 1 : index iter_args(%31 = %12 : f32) {
              %32 = d_memref.load %16[%23, %30] : !d_memref.memref<[%8, %4], f32, offset: 0, strides: [%15, %11]> -> f32
              %33 = d_memref.load %17[%30, %28] : !d_memref.memref<[%4, %9], f32, offset: 0, strides: [%14, %11]> -> f32
              %34 = "arith.mulf"(%32, %33) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %35 = "arith.addf"(%31, %34) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %35 : (f32)
            }
            d_memref.store %29, %18[%23, %28] : f32, !d_memref.memref<[%8, %9], f32, offset: 0, strides: [%14, %11]>
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
