// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | filecheck %s

builtin.module {
  func.func @shifted_bound_lowers_directly(%ub: index, %out: memref<?xf32>) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0 + 4)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @shifted_bound_lowers_directly
// CHECK: "llvm.add"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK-NOT: d_affine.for
