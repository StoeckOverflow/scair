// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-memref-bounds-check | filecheck %s -DFILE=%s

builtin.module {
  %len = "arith.constant"() <{value = 8 : index}> : () -> index
  %three = "arith.constant"() <{value = 3 : index}> : () -> index
  %two = "arith.constant"() <{value = 2 : index}> : () -> index
  %dyn = "arith.constant"() <{value = 6 : index}> : () -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
  %m = d_memref.alloc : () -> !d_memref.memref<[%len], i32>
  %loop_m = d_memref.alloc : () -> !d_memref.memref<[%dyn], i32>
  %z = "arith.constant"() <{value = 0 : i32}> : () -> i32
  d_memref.store %z, %m[%three] : i32, !d_memref.memref<[%len], i32>
  %loaded = d_memref.load %m[%three] : !d_memref.memref<[%len], i32> -> i32
  d_memref.store %loaded, %m[%dyn] : i32, !d_memref.memref<[%len], i32>
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%dyn) step 1 : index {
    d_memref.store %z, %loop_m[%iv] : i32, !d_memref.memref<[%dyn], i32>
    d_affine.yield
  }
  %sv = d_memref.subview %m[%three][%two][%c1] : !d_memref.memref<[%len], i32> -> !d_memref.memref<[%two], i32>
  d_memref.dealloc %sv : !d_memref.memref<[%two], i32>
  d_memref.dealloc %loop_m : !d_memref.memref<[%dyn], i32>
  d_memref.dealloc %m : !d_memref.memref<[%len], i32>
}

// CHECK: #[[ID_MAP:.*]] = affine_map<(d0)[] -> (d0)>
// CHECK-LABEL: builtin.module
// CHECK: %[[LEN:[0-9]+]] = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK: %[[THREE:[0-9]+]] = "arith.constant"() <{value = 3 : index}> : () -> index
// CHECK: %[[TWO:[0-9]+]] = "arith.constant"() <{value = 2 : index}> : () -> index
// CHECK: %[[DYN:[0-9]+]] = "arith.constant"() <{value = 6 : index}> : () -> index
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[C1:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK: %[[M:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[LEN]]], i32>
// CHECK: %[[LOOP_M:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[DYN]]], i32>
// CHECK: %[[Z:[0-9]+]] = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CHECK: d_memref.store %[[Z]], %[[M]][%[[THREE]]] : i32, !d_memref.memref<[%[[LEN]]], i32>
// CHECK: %[[LOADED:[0-9]+]] = d_memref.load %[[M]][%[[THREE]]] : !d_memref.memref<[%[[LEN]]], i32> -> i32
// CHECK: d_memref.store %[[LOADED]], %[[M]][%[[DYN]]] : i32, !d_memref.memref<[%[[LEN]]], i32>
// CHECK: d_affine.for %[[IV:[0-9]+]] = #[[ID_MAP]](%[[C0]]) to #[[ID_MAP]](%[[DYN]]) step 1 : index {
// CHECK-NEXT:   d_memref.store %[[Z]], %[[LOOP_M]][%[[IV]]] : i32, !d_memref.memref<[%[[DYN]]], i32>
// CHECK-NEXT:   d_affine.yield
// CHECK-NEXT: }
// CHECK: %[[SV:[0-9]+]] = d_memref.subview %[[M]][%[[THREE]]][%[[TWO]]][%[[C1]]] : !d_memref.memref<[%[[LEN]]], i32> -> !d_memref.memref<[%[[TWO]]], i32>
// CHECK: d_memref.dealloc %[[SV]] : !d_memref.memref<[%[[TWO]]], i32>
// CHECK: d_memref.dealloc %[[LOOP_M]] : !d_memref.memref<[%[[DYN]]], i32>
// CHECK: d_memref.dealloc %[[M]] : !d_memref.memref<[%[[LEN]]], i32>
