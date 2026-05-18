module {
  func.func @ordinary_same_shape_different_ssa(
      %arg0: tensor<?x?xf32>,
      %arg1: tensor<?x?xf32>,
      %arg2: tensor<?x?xf32>,
      %arg3: tensor<?x?xf32>,
      %arg4: tensor<?x?xf32>,
      %arg5: tensor<?x?xf32>) -> index {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %a0 = tensor.dim %arg0, %c0 : tensor<?x?xf32>
    %a1 = tensor.dim %arg0, %c1 : tensor<?x?xf32>
    %b0 = tensor.dim %arg1, %c0 : tensor<?x?xf32>
    %b1 = tensor.dim %arg1, %c1 : tensor<?x?xf32>
    %c0_dim = tensor.dim %arg2, %c0 : tensor<?x?xf32>
    %c1_dim = tensor.dim %arg2, %c1 : tensor<?x?xf32>
    %d0 = tensor.dim %arg3, %c0 : tensor<?x?xf32>
    %d1 = tensor.dim %arg3, %c1 : tensor<?x?xf32>
    %e0 = tensor.dim %arg4, %c0 : tensor<?x?xf32>
    %e1 = tensor.dim %arg4, %c1 : tensor<?x?xf32>
    %f0 = tensor.dim %arg5, %c0 : tensor<?x?xf32>
    %f1 = tensor.dim %arg5, %c1 : tensor<?x?xf32>
    %size_a = arith.muli %a0, %a1 : index
    %size_b = arith.muli %b0, %b1 : index
    %size_c = arith.muli %c0_dim, %c1_dim : index
    %size_d = arith.muli %d0, %d1 : index
    %size_e = arith.muli %e0, %e1 : index
    %size_f = arith.muli %f0, %f1 : index
    %s0 = arith.addi %size_a, %size_b : index
    %s1 = arith.addi %s0, %size_c : index
    %s2 = arith.addi %s1, %size_d : index
    %s3 = arith.addi %s2, %size_e : index
    %s4 = arith.addi %s3, %size_f : index
    return %s4 : index
  }
}
