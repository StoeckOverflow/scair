module {
  func.func @ordinary_same_shape_different_ssa(%arg0: tensor<?x?xf32>, %arg1: tensor<?x?xf32>, %arg2: tensor<?x?xf32>, %arg3: tensor<?x?xf32>, %arg4: tensor<?x?xf32>, %arg5: tensor<?x?xf32>) -> index {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %dim = tensor.dim %arg0, %c0 : tensor<?x?xf32>
    %dim_0 = tensor.dim %arg0, %c1 : tensor<?x?xf32>
    %dim_1 = tensor.dim %arg1, %c0 : tensor<?x?xf32>
    %dim_2 = tensor.dim %arg1, %c1 : tensor<?x?xf32>
    %dim_3 = tensor.dim %arg2, %c0 : tensor<?x?xf32>
    %dim_4 = tensor.dim %arg2, %c1 : tensor<?x?xf32>
    %dim_5 = tensor.dim %arg3, %c0 : tensor<?x?xf32>
    %dim_6 = tensor.dim %arg3, %c1 : tensor<?x?xf32>
    %dim_7 = tensor.dim %arg4, %c0 : tensor<?x?xf32>
    %dim_8 = tensor.dim %arg4, %c1 : tensor<?x?xf32>
    %dim_9 = tensor.dim %arg5, %c0 : tensor<?x?xf32>
    %dim_10 = tensor.dim %arg5, %c1 : tensor<?x?xf32>
    %0 = arith.muli %dim, %dim_0 : index
    %1 = arith.muli %dim_1, %dim_2 : index
    %2 = arith.muli %dim_3, %dim_4 : index
    %3 = arith.muli %dim_5, %dim_6 : index
    %4 = arith.muli %dim_7, %dim_8 : index
    %5 = arith.muli %dim_9, %dim_10 : index
    %6 = arith.addi %0, %1 : index
    %7 = arith.addi %6, %2 : index
    %8 = arith.addi %7, %3 : index
    %9 = arith.addi %8, %4 : index
    %10 = arith.addi %9, %5 : index
    return %10 : index
  }
}

