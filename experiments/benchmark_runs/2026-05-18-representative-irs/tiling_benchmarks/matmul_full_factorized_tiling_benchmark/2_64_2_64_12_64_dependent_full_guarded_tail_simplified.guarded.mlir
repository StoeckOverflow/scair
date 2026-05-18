#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @matmul_full_factorized_tiling(%0: !dtensor.nat, %1: !dtensor.posnat, %2: !dtensor.nat, %3: !dtensor.posnat, %4: !dtensor.nat, %5: !dtensor.posnat, %6: !d_memref.memref<[], f32>, %7: !d_memref.memref<[], f32>, %8: !d_memref.memref<[], f32>) attributes {scair.emit_bare_interface = true} {
    %9 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %10 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %11 = "dtensor.nat.mul"(%4, %5) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %12 = "arith.constant"() <{value = 0 : index}> : () -> index
    %13 = "arith.constant"() <{value = 1 : index}> : () -> index
    %14 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %15 = "dtensor.shape.to_index"(%9) : (!dtensor.nat) -> index
    %16 = "dtensor.shape.to_index"(%10) : (!dtensor.nat) -> index
    %17 = "dtensor.shape.to_index"(%11) : (!dtensor.nat) -> index
    %18 = d_memref.reinterpret_cast %6
    : !d_memref.memref<[], f32> to !d_memref.memref<[%9, %11], f32, offset: 0, strides: [%17, %13]>
    %19 = d_memref.reinterpret_cast %7
    : !d_memref.memref<[], f32> to !d_memref.memref<[%11, %10], f32, offset: 0, strides: [%16, %13]>
    %20 = d_memref.reinterpret_cast %8
    : !d_memref.memref<[], f32> to !d_memref.memref<[%9, %10], f32, offset: 0, strides: [%16, %13]>
    %21 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
    d_affine.for %22 = #map(%12) to #map(%15) step %21 : index {
      %23 = "arith.addi"(%22, %21) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
      %24 = "arith.minsi"(%23, %15) : (index, index) -> index
      d_affine.for %25 = #map(%22) to #map(%24) step 1 : index {
        %26 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
        d_affine.for %27 = #map(%12) to #map(%16) step %26 : index {
          %28 = "arith.addi"(%27, %26) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          %29 = "arith.minsi"(%28, %16) : (index, index) -> index
          d_affine.for %30 = #map(%27) to #map(%29) step 1 : index {
            %31 = "dtensor.shape.to_index"(%5) : (!dtensor.posnat) -> index
            %32 = d_affine.for %33 = #map(%12) to #map(%17) step %31 : index iter_args(%34 = %14 : f32) {
              %35 = "arith.addi"(%33, %31) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
              %36 = "arith.minsi"(%35, %17) : (index, index) -> index
              %37 = d_affine.for %38 = #map(%33) to #map(%36) step 1 : i32 iter_args(%39 = %34 : f32) {
                %40 = d_memref.load %18[%25, %38] : !d_memref.memref<[%9, %11], f32, offset: 0, strides: [%17, %13]> -> f32
                %41 = d_memref.load %19[%38, %30] : !d_memref.memref<[%11, %10], f32, offset: 0, strides: [%16, %13]> -> f32
                %42 = "arith.mulf"(%40, %41) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                %43 = "arith.addf"(%39, %42) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                d_affine.yield %43 : (f32)
              }
              d_affine.yield %37 : (f32)
            }
            d_memref.store %32, %20[%25, %30] : f32, !d_memref.memref<[%9, %10], f32, offset: 0, strides: [%16, %13]>
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
