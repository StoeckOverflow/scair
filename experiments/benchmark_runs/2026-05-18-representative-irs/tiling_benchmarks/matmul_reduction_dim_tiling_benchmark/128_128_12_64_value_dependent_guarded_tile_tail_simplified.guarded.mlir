#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @matmul_reduction_dim_tiling(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat, %3: !dtensor.posnat, %4: !d_memref.memref<[], f32>, %5: !d_memref.memref<[], f32>, %6: !d_memref.memref<[], f32>) attributes {scair.emit_bare_interface = true} {
    %7 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %8 = "arith.constant"() <{value = 0 : index}> : () -> index
    %9 = "arith.constant"() <{value = 1 : index}> : () -> index
    %10 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %11 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
    %12 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %13 = "dtensor.shape.to_index"(%7) : (!dtensor.nat) -> index
    %14 = d_memref.reinterpret_cast %4
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %7], f32, offset: 0, strides: [%13, %9]>
    %15 = d_memref.reinterpret_cast %5
    : !d_memref.memref<[], f32> to !d_memref.memref<[%7, %1], f32, offset: 0, strides: [%12, %9]>
    %16 = d_memref.reinterpret_cast %6
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%12, %9]>
    d_affine.for %17 = #map(%8) to #map(%11) step 1 : index {
      d_affine.for %18 = #map(%8) to #map(%12) step 1 : index {
        %19 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
        %20 = d_affine.for %21 = #map(%8) to #map(%13) step %19 : index iter_args(%22 = %10 : f32) {
          %23 = "arith.addi"(%21, %19) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          %24 = "arith.minsi"(%23, %13) : (index, index) -> index
          %25 = d_affine.for %26 = #map(%21) to #map(%24) step 1 : i32 iter_args(%27 = %22 : f32) {
            %28 = d_memref.load %14[%17, %26] : !d_memref.memref<[%0, %7], f32, offset: 0, strides: [%13, %9]> -> f32
            %29 = d_memref.load %15[%26, %18] : !d_memref.memref<[%7, %1], f32, offset: 0, strides: [%12, %9]> -> f32
            %30 = "arith.mulf"(%28, %29) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            %31 = "arith.addf"(%27, %30) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            d_affine.yield %31 : (f32)
          }
          d_affine.yield %25 : (f32)
        }
        d_memref.store %20, %16[%17, %18] : f32, !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%12, %9]>
        d_affine.yield
      }
      d_affine.yield
    }
    func.return
  }
}
