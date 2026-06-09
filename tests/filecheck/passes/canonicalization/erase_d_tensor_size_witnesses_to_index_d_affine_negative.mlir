// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-size-witnesses-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_d_affine_still_present(%k0: index, %out: memref<?xf32>) {
    %k0_size = "d_tensor.size.import"(%k0) : (index) -> !d_tensor.size
    %k1_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %k_size = "d_tensor.size.mul"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: erase-d-tensor-size-witnesses-to-index cannot run while d_affine.for remains
