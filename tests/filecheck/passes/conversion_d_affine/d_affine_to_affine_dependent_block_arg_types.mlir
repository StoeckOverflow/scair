// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-affine-to-affine-compatible | filecheck %s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-affine-to-affine-compatible | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  func.func @iter_arg_dependent_type(
    %lb: index,
    %ub: index,
    %n: !d_tensor.nat,
    %init: !d_memref.memref<[%n], f32>
  ) -> !d_memref.memref<[%n], f32> {
    %r = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : !d_memref.memref<[%n], f32>) {
      d_affine.yield %acc : (!d_memref.memref<[%n], f32>)
    }
    func.return %r : !d_memref.memref<[%n], f32>
  }
}

// CHECK-LABEL: func.func @iter_arg_dependent_type(
// CHECK-SAME: %{{[0-9]+}}: index, %{{[0-9]+}}: index, %[[N:[0-9]+]]: !d_tensor.nat, %[[INIT:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>
// CHECK-SAME: -> !d_memref.memref<[%[[N]]], f32>
// CHECK: %[[R:[0-9]+]] = affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 iter_args(%[[ACC:[0-9]+]] = %[[INIT]]) -> (!d_memref.memref<[%[[N]]], f32>) {
// CHECK: affine.yield %[[ACC]] : !d_memref.memref<[%[[N]]], f32>
// CHECK: func.return %[[R]] : !d_memref.memref<[%[[N]]], f32>
// CHECK-NOT: d_affine
