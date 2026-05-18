builtin.module {
  func.func @tensor_shape_identity(%0: tensor<4xi32>) -> tensor<4xi32> {
    %1 = "tlam.vlambda"() ({
    ^bb0(%2: tensor<4xi32>):
      "tlam.vreturn"(%2) : (tensor<4xi32>) -> ()
    }) : () -> !tlam.fun<tensor<4xi32>, tensor<4xi32>>
    %2 = "tlam.vapply"(%1, %0) : (!tlam.fun<tensor<4xi32>, tensor<4xi32>>, tensor<4xi32>) -> tensor<4xi32>
    func.return %2 : tensor<4xi32>
  }
}
