// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %lb_nat = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
    %ub_nat = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
    %ub = "d_tensor.shape.to_index"(%ub_nat) : (!d_tensor.nat) -> index
    %lb = "d_tensor.shape.to_index"(%lb_nat) : (!d_tensor.nat) -> index
    %one = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = d_memref.alloc : () -> !d_memref.memref<[%ub_nat], i32>
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 {
      d_memref.store %c7, %m[%iv] : i32, !d_memref.memref<[%ub_nat], i32>
      d_affine.yield
    }
    %r = d_memref.load %m[%one] : !d_memref.memref<[%ub_nat], i32> -> i32
    func.return %r : i32
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> i32 {
// IR: %0 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// IR: %1 = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
// IR: %2 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// IR: %3 = "d_tensor.shape.to_index"(%0) : (!d_tensor.nat) -> index
// IR: %4 = "arith.constant"() <{value = 1 : index}> : () -> index
// IR: %5 = d_memref.alloc : () -> !d_memref.memref<[%1], i32>
// IR: %6 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// IR: d_affine.for %7 = #map(%3) to #map(%2) step 1 : i32 {
// IR: d_memref.store %6, %5[%7] : i32, !d_memref.memref<[%1], i32>
// IR: d_affine.yield
// IR: }
// IR: %8 = d_memref.load %5[%4] : !d_memref.memref<[%1], i32> -> i32
// IR: func.return %8 : i32

// EXEC: Result: 7
