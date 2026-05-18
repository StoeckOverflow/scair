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
      d_affine.for %22 = #map(%20) to #map(%21) step 1 : index {
        %23 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
        d_affine.for %24 = #map(%10) to #map(%14) step %23 : index {
          %25 = "arith.addi"(%24, %23) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          d_affine.for %26 = #map(%24) to #map(%25) step 1 : index {
            %27 = d_affine.for %28 = #map(%10) to #map(%15) step 1 : index iter_args(%29 = %12 : f32) {
              %30 = d_memref.load %16[%22, %28] : !d_memref.memref<[%8, %4], f32, offset: 0, strides: [%15, %11]> -> f32
              %31 = d_memref.load %17[%28, %26] : !d_memref.memref<[%4, %9], f32, offset: 0, strides: [%14, %11]> -> f32
              %32 = "arith.mulf"(%30, %31) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %33 = "arith.addf"(%29, %32) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %33 : (f32)
            }
            d_memref.store %27, %18[%22, %26] : f32, !d_memref.memref<[%8, %9], f32, offset: 0, strides: [%14, %11]>
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
