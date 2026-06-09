// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check | filecheck %s -DFILE=%s

builtin.module {
  %len = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %three = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %two = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %three_i = "arith.constant"() <{value = 3 : index}> : () -> index
  %two_i = "arith.constant"() <{value = 2 : index}> : () -> index
  %dyn_i = "test.dynamic_index"() : () -> index
  %dyn = "d_tensor.size.import"(%dyn_i) : (index) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
  %m = d_memref.alloc : () -> !d_memref.memref<[%len], i32>
  %loop_m = d_memref.alloc : () -> !d_memref.memref<[%dyn], i32>
  %z = "arith.constant"() <{value = 0 : i32}> : () -> i32
  d_memref.store %z, %m[%three_i] : i32, !d_memref.memref<[%len], i32>
  %loaded = d_memref.load %m[%three_i] : !d_memref.memref<[%len], i32> -> i32
  d_memref.store %loaded, %m[%dyn_i] : i32, !d_memref.memref<[%len], i32>
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%dyn) step 1 : index {
    d_memref.store %z, %loop_m[%iv] : i32, !d_memref.memref<[%dyn], i32>
    d_affine.yield
  }
  %sv = d_memref.subview %m[%three_i][%two][%c1] : !d_memref.memref<[%len], i32> -> !d_memref.memref<[%two], i32>
  d_memref.dealloc %sv : !d_memref.memref<[%two], i32>
  d_memref.dealloc %loop_m : !d_memref.memref<[%dyn], i32>
  d_memref.dealloc %m : !d_memref.memref<[%len], i32>
}

// CHECK: #[[ID_MAP:.*]] = affine_map<(d0)[] -> (d0)>
// CHECK-LABEL: builtin.module
// CHECK: %[[LEN:[0-9]+]] = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// CHECK: %[[THREE:[0-9]+]] = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// CHECK: %[[TWO:[0-9]+]] = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// CHECK: %[[THREE_IDX:[0-9]+]] = "arith.constant"() <{value = 3 : index}> : () -> index
// CHECK: %[[TWO_IDX:[0-9]+]] = "arith.constant"() <{value = 2 : index}> : () -> index
// CHECK: %[[DYN_IDX:[0-9]+]] = "test.dynamic_index"() : () -> index
// CHECK: %[[DYN:[0-9]+]] = "d_tensor.size.import"(%[[DYN_IDX]]) : (index) -> !d_tensor.size
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[C1:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK: %[[M:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[LEN]]], i32>
// CHECK: %[[LOOP_M:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[DYN]]], i32>
// CHECK: %[[Z:[0-9]+]] = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CHECK: d_memref.store %[[Z]], %[[M]][%[[THREE_IDX]]] : i32, !d_memref.memref<[%[[LEN]]], i32>
// CHECK: %[[LOADED:[0-9]+]] = d_memref.load %[[M]][%[[THREE_IDX]]] : !d_memref.memref<[%[[LEN]]], i32> -> i32
// CHECK: d_memref.store %[[LOADED]], %[[M]][%[[DYN_IDX]]] : i32, !d_memref.memref<[%[[LEN]]], i32>
// CHECK: d_affine.for %[[IV:[0-9]+]] = #[[ID_MAP]](%[[C0]]) to #[[ID_MAP]](%[[DYN]]) step 1 : index {
// CHECK-NEXT:   d_memref.store %[[Z]], %[[LOOP_M]][%[[IV]]] : i32, !d_memref.memref<[%[[DYN]]], i32>
// CHECK-NEXT:   d_affine.yield
// CHECK-NEXT: }
// CHECK: %[[SV:[0-9]+]] = d_memref.subview %[[M]][%[[THREE_IDX]]][%[[TWO]]][%[[C1]]] : !d_memref.memref<[%[[LEN]]], i32> -> !d_memref.memref<[%[[TWO]]], i32>
// CHECK: d_memref.dealloc %[[SV]] : !d_memref.memref<[%[[TWO]]], i32>
// CHECK: d_memref.dealloc %[[LOOP_M]] : !d_memref.memref<[%[[DYN]]], i32>
// CHECK: d_memref.dealloc %[[M]] : !d_memref.memref<[%[[LEN]]], i32>
