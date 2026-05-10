builtin.module {
  func.func @tensor_shape_identity(%x: tensor<4xi32>) -> tensor<4xi32> {
    %id = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%arg: !value<%T>):
        "tlam.vreturn"(%arg) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %spec = "tlam.tapply"(%id) <{tyArg = tensor<4xi32>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> !tlam.fun<tensor<4xi32>, tensor<4xi32>>
    %r = "tlam.vapply"(%spec, %x)
      : (!tlam.fun<tensor<4xi32>, tensor<4xi32>>, tensor<4xi32>)
        -> tensor<4xi32>
    func.return %r : tensor<4xi32>
  }
}
