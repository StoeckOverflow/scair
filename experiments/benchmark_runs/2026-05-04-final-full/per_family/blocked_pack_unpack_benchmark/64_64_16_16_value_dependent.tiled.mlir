#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @blocked_pack(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat, %3: !dtensor.nat, %4: !d_memref.memref<[], i64>, %5: !d_memref.memref<[], i64>) attributes {scair.emit_bare_interface = true} {
    %6 = "dtensor.nat.mul"(%0, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %7 = "dtensor.nat.mul"(%1, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %8 = "arith.constant"() <{value = 0 : index}> : () -> index
    %9 = "arith.constant"() <{value = 1 : index}> : () -> index
    %10 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
    %11 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %12 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
    %13 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %14 = "dtensor.shape.to_index"(%7) : (!dtensor.nat) -> index
    %15 = "arith.muli"(%12, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %16 = "arith.muli"(%11, %15) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %17 = d_memref.reinterpret_cast %4
    : !d_memref.memref<[], i64> to !d_memref.memref<[%6, %7], i64, offset: 0, strides: [%14, %9]>
    %18 = d_memref.reinterpret_cast %5
    : !d_memref.memref<[], i64> to !d_memref.memref<[%0, %1, %2, %3], i64, offset: 0, strides: [%16, %15, %13, %9]>
    d_affine.for %19 = #map(%8) to #map(%10) step 1 : index {
%20 = "arith.muli"(%19, %12) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
d_affine.for %21 = #map(%8) to #map(%11) step 1 : index {
%22 = "arith.muli"(%21, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
d_affine.for %23 = #map(%8) to #map(%12) step 1 : index {
%24 = "arith.addi"(%20, %23) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
d_affine.for %25 = #map(%8) to #map(%13) step 1 : index {
%26 = "arith.addi"(%22, %25) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%27 = d_memref.load %17[%24, %26] : !d_memref.memref<[%6, %7], i64, offset: 0, strides: [%14, %9]> -> i64
d_memref.store %27, %18[%19, %21, %23, %25] : i64, !d_memref.memref<[%0, %1, %2, %3], i64, offset: 0, strides: [%16, %15, %13, %9]>
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
