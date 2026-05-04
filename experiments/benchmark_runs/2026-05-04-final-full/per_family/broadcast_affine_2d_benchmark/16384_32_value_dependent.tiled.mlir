#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @broadcast_affine_2d(%0: !dtensor.nat, %1: !dtensor.nat, %2: !d_memref.memref<[], i64>, %3: !d_memref.memref<[], i64>, %4: !d_memref.memref<[], i64>, %5: !d_memref.memref<[], i64>) attributes {scair.emit_bare_interface = true} {
    %6 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
    %8 = "arith.constant"() <{value = 1 : index}> : () -> index
    %9 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
    %10 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %11 = d_memref.reinterpret_cast %2
    : !d_memref.memref<[], i64> to !d_memref.memref<[%6], i64, offset: 0, strides: [%8]>
    %12 = d_memref.reinterpret_cast %3
    : !d_memref.memref<[], i64> to !d_memref.memref<[%1], i64, offset: 0, strides: [%8]>
    %13 = d_memref.reinterpret_cast %4
    : !d_memref.memref<[], i64> to !d_memref.memref<[%1], i64, offset: 0, strides: [%8]>
    %14 = d_memref.reinterpret_cast %5
    : !d_memref.memref<[], i64> to !d_memref.memref<[%6], i64, offset: 0, strides: [%8]>
    d_affine.for %15 = #map(%7) to #map(%9) step 1 : index {
%16 = "arith.muli"(%15, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
d_affine.for %17 = #map(%7) to #map(%10) step 1 : index {
%18 = "arith.addi"(%16, %17) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%19 = d_memref.load %11[%18] : !d_memref.memref<[%6], i64, offset: 0, strides: [%8]> -> i64
%20 = d_memref.load %12[%17] : !d_memref.memref<[%1], i64, offset: 0, strides: [%8]> -> i64
%21 = d_memref.load %13[%17] : !d_memref.memref<[%1], i64, offset: 0, strides: [%8]> -> i64
%22 = "arith.muli"(%19, %20) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
%23 = "arith.addi"(%22, %21) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
d_memref.store %23, %14[%18] : i64, !d_memref.memref<[%6], i64, offset: 0, strides: [%8]>
d_affine.yield
      }
d_affine.yield
    }
    func.return
  }
}
