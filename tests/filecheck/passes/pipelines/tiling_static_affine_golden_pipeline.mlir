// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products,dependent-product-loop-exact-tile,d-affine-to-affine-compatible,canonicalize,cse,dce | filecheck %s --implicit-check-not=d_tensor. --implicit-check-not=d_affine.for --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not="step %"

builtin.module {
  func.func @static_affine_exact(%k0_idx: index, %out: memref<?xf32>) {
    %k1 = "arith.constant"() <{value = 3 : index}> : () -> index
    %ub = "arith.muli"(%k0_idx, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @static_affine_exact
// CHECK: "arith.muli"
// CHECK: affine.for %[[TILE:[0-9]+]] = {{.*}} step 3
// CHECK: affine.for %[[P:[0-9]+]] = {{.*}} step 1
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
