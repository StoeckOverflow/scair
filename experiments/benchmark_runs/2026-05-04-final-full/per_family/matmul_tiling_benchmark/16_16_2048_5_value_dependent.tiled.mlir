#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @matmul_tiling(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat, %3: !dtensor.nat, %4: !d_memref.memref<[], f32>, %5: !d_memref.memref<[], f32>, %6: !d_memref.memref<[], f32>) attributes {scair.emit_bare_interface = true} {
    %7 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
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
%19 = "arith.constant"() <{value = 0 : index}> : () -> index
%20 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
%21 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
%22 = d_affine.for %23 = #map(%19) to #map(%20) step 1 : i32 iter_args(%24 = %10 : f32) {
%25 = d_affine.for %26 = #map(%19) to #map(%21) step 1 : i32 iter_args(%27 = %24 : f32) {
%28 = "arith.muli"(%23, %21) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%29 = "arith.addi"(%28, %26) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%30 = d_memref.load %14[%17, %29] : !d_memref.memref<[%0, %7], f32, offset: 0, strides: [%13, %9]> -> f32
%31 = d_memref.load %15[%29, %18] : !d_memref.memref<[%7, %1], f32, offset: 0, strides: [%12, %9]> -> f32
%32 = "arith.mulf"(%30, %31) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%33 = "arith.addf"(%27, %32) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_affine.yield %33 : (f32)
          }
d_affine.yield %25 : (f32)
        }
d_memref.store %22, %16[%17, %18] : f32, !d_memref.memref<[%0, %1], f32, offset: 0, strides: [%12, %9]>
d_affine.yield
      }
d_affine.yield
    }
    func.return
  }
}
