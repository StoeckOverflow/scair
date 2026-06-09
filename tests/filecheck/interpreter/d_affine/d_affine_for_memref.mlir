// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %lb = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 5 : index}> : () -> index
    %one = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = d_memref.alloc : () -> !d_memref.memref<[%ub], i32>
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 {
      d_memref.store %c7, %m[%iv] : i32, !d_memref.memref<[%ub], i32>
      d_affine.yield
    }
    %r = d_memref.load %m[%one] : !d_memref.memref<[%ub], i32> -> i32
    func.return %r : i32
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> i32 {
// IR: %0 = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %1 = "arith.constant"() <{value = 5 : index}> : () -> index
// IR: %2 = "arith.constant"() <{value = 1 : index}> : () -> index
// IR: %3 = d_memref.alloc : () -> !d_memref.memref<[%1], i32>
// IR: %4 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// IR: d_affine.for %5 = #map(%0) to #map(%1) step 1 : i32 {
// IR: d_memref.store %4, %3[%5] : i32, !d_memref.memref<[%1], i32>
// IR: d_affine.yield
// IR: }
// IR: %6 = d_memref.load %3[%2] : !d_memref.memref<[%1], i32> -> i32
// IR: func.return %6 : i32

// EXEC: Result: 7
