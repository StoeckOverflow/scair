// RUN: ! scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm 2>&1 | filecheck %s

builtin.module {
  func.func @unsupported_bound_needs_affine_bridge(%ub: index, %out: memref<?xf32>) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0 mod 4)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: lower-refined-control-flow-to-llvm cannot lower d_affine.for
// CHECK: mod affine expressions are not supported
// CHECK: d-affine-to-affine-compatible
