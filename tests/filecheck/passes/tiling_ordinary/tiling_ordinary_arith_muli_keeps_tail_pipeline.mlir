// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-affine-product-loop-tile-with-tail:3,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s --implicit-check-not=d_tensor.size.mul --implicit-check-not=d_affine.for

builtin.module {
  func.func @ordinary_arith_muli_keeps_tail(%k0: index, %k1: index, %out: memref<?xf32>) {
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @ordinary_arith_muli_keeps_tail
// CHECK: "arith.muli"
// CHECK: affine.for %[[TILE:[0-9]+]] = {{.*}} step 3
// CHECK: affine.for %[[P:[0-9]+]] = {{.*}} to min {{.*}} step 1
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
