// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %lb_size = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
    %ub_size = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
    %one = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = d_memref.alloc : () -> !d_memref.memref<[%ub_size], i32>
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb_size) to affine_map<(d0) -> (d0)>(%ub_size) step 1 : i32 {
      d_memref.store %c7, %m[%iv] : i32, !d_memref.memref<[%ub_size], i32>
      d_affine.yield
    }
    %r = d_memref.load %m[%one] : !d_memref.memref<[%ub_size], i32> -> i32
    func.return %r : i32
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> i32 {
// IR: %0 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// IR: %1 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// IR: %[[ONE:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// IR: %[[MEM:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%1], i32>
// IR: %[[C7:[0-9]+]] = "arith.constant"() <{value = 7 : i32}> : () -> i32
// IR: d_affine.for %[[IV:[0-9]+]] = #map(%0) to #map(%1) step 1 : i32 {
// IR: d_memref.store %[[C7]], %[[MEM]][%[[IV]]] : i32, !d_memref.memref<[%1], i32>
// IR: d_affine.yield
// IR: }
// IR: %[[LOAD:[0-9]+]] = d_memref.load %[[MEM]][%[[ONE]]] : !d_memref.memref<[%1], i32> -> i32
// IR: func.return %[[LOAD]] : i32

// EXEC: Result: 7
