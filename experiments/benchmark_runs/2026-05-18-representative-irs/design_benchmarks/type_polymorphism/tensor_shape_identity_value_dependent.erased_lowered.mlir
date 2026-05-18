builtin.module {
  func.func @lifted_1(%0: tensor<4xi32>) -> tensor<4xi32> {
    func.return %0 : tensor<4xi32>
  }
  func.func @tensor_shape_identity(%0: tensor<4xi32>) -> tensor<4xi32> {
    func.return %0 : tensor<4xi32>
  }
}
