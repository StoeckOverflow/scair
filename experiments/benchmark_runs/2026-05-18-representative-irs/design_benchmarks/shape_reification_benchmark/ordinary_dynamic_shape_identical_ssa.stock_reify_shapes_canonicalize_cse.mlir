module {
  func.func @ordinary_identical_ssa(%arg0: tensor<?x?xf32>) -> index {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %dim = tensor.dim %arg0, %c0 : tensor<?x?xf32>
    %dim_0 = tensor.dim %arg0, %c1 : tensor<?x?xf32>
    %0 = arith.muli %dim, %dim_0 : index
    %1 = arith.addi %0, %0 : index
    %2 = arith.addi %1, %0 : index
    %3 = arith.addi %2, %0 : index
    %4 = arith.addi %3, %0 : index
    %5 = arith.addi %4, %0 : index
    return %5 : index
  }
}

