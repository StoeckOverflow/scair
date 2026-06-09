// RUN: ! scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-size-witnesses-to-index 2>&1 | filecheck %s

builtin.module {
  func.func @erase_with_d_affine_min_still_present(%k0: index, %k1: index) -> index {
    %k0_size = "d_tensor.size.import"(%k0) : (index) -> !d_tensor.size
    %k1_size = "d_tensor.size.import"(%k1) : (index) -> !d_tensor.size
    %k_size = "d_tensor.size.add"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %clamped = d_affine.min affine_map<(d0) -> (d0)>(%k_size)[] : (index)[] -> index
    "func.return"(%clamped) : (index) -> ()
  }
}

// CHECK: erase-d-tensor-size-witnesses-to-index cannot run while d_affine.min remains
