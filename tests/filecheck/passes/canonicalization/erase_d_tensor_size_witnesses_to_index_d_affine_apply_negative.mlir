// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-size-witnesses-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_d_affine_apply_still_present(%k0: index) -> index {
    %k0_size = "d_tensor.size.import"(%k0) : (index) -> !d_tensor.size
    %k1_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %k_size = "d_tensor.size.mul"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %shifted = d_affine.apply affine_map<(d0) -> (d0 + 1)>(%k_size)[] : (index)[] -> index
    "func.return"(%shifted) : (index) -> ()
  }
}

// CHECK: erase-d-tensor-size-witnesses-to-index cannot run while d_affine.apply remains
