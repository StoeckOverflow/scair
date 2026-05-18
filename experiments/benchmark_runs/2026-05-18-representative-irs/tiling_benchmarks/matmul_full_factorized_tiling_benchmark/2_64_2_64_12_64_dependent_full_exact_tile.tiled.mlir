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
      d_affine.for %24 = #map(%22) to #map(%23) step 1 : index {
        %25 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
        d_affine.for %26 = #map(%12) to #map(%16) step %25 : index {
          %27 = "arith.addi"(%26, %25) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          d_affine.for %28 = #map(%26) to #map(%27) step 1 : index {
            %29 = "dtensor.shape.to_index"(%5) : (!dtensor.posnat) -> index
            %30 = d_affine.for %31 = #map(%12) to #map(%17) step %29 : index iter_args(%32 = %14 : f32) {
              %33 = "arith.addi"(%31, %29) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
              %34 = d_affine.for %35 = #map(%31) to #map(%33) step 1 : i32 iter_args(%36 = %32 : f32) {
                %37 = d_memref.load %18[%24, %35] : !d_memref.memref<[%9, %11], f32, offset: 0, strides: [%17, %13]> -> f32
                %38 = d_memref.load %19[%35, %28] : !d_memref.memref<[%11, %10], f32, offset: 0, strides: [%16, %13]> -> f32
                %39 = "arith.mulf"(%37, %38) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                %40 = "arith.addf"(%36, %39) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                d_affine.yield %40 : (f32)
              }
              d_affine.yield %34 : (f32)
            }
            d_memref.store %30, %20[%24, %28] : f32, !d_memref.memref<[%9, %10], f32, offset: 0, strides: [%16, %13]>
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
