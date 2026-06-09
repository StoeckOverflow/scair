builtin.module {
  func.func @dependent_same_shape_different_ssa(
      %m: index,
      %n: index,
      %a: !d_tensor.tensor<[%m, %n], f32>,
      %b: !d_tensor.tensor<[%m, %n], f32>,
      %c: !d_tensor.tensor<[%m, %n], f32>,
      %d: !d_tensor.tensor<[%m, %n], f32>,
      %e: !d_tensor.tensor<[%m, %n], f32>,
      %f: !d_tensor.tensor<[%m, %n], f32>) -> index {
    %a0_ref = "d_tensor.dim"(%a) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %b0_ref = "d_tensor.dim"(%b) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %c0_ref = "d_tensor.dim"(%c) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %d0_ref = "d_tensor.dim"(%d) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %e0_ref = "d_tensor.dim"(%e) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %f0_ref = "d_tensor.dim"(%f) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %a1_ref = "d_tensor.dim"(%a) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %b1_ref = "d_tensor.dim"(%b) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %c1_ref = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %d1_ref = "d_tensor.dim"(%d) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %e1_ref = "d_tensor.dim"(%e) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %f1_ref = "d_tensor.dim"(%f) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>

    %a0 = "builtin.unrealized_conversion_cast"(%a0_ref) : (!value<%m>) -> index
    %b0 = "builtin.unrealized_conversion_cast"(%b0_ref) : (!value<%m>) -> index
    %c0 = "builtin.unrealized_conversion_cast"(%c0_ref) : (!value<%m>) -> index
    %d0 = "builtin.unrealized_conversion_cast"(%d0_ref) : (!value<%m>) -> index
    %e0 = "builtin.unrealized_conversion_cast"(%e0_ref) : (!value<%m>) -> index
    %f0 = "builtin.unrealized_conversion_cast"(%f0_ref) : (!value<%m>) -> index
    %a1 = "builtin.unrealized_conversion_cast"(%a1_ref) : (!value<%n>) -> index
    %b1 = "builtin.unrealized_conversion_cast"(%b1_ref) : (!value<%n>) -> index
    %c1 = "builtin.unrealized_conversion_cast"(%c1_ref) : (!value<%n>) -> index
    %d1 = "builtin.unrealized_conversion_cast"(%d1_ref) : (!value<%n>) -> index
    %e1 = "builtin.unrealized_conversion_cast"(%e1_ref) : (!value<%n>) -> index
    %f1 = "builtin.unrealized_conversion_cast"(%f1_ref) : (!value<%n>) -> index

    %size_a = "arith.muli"(%a0, %a1) : (index, index) -> index
    %size_b = "arith.muli"(%b0, %b1) : (index, index) -> index
    %size_c = "arith.muli"(%c0, %c1) : (index, index) -> index
    %size_d = "arith.muli"(%d0, %d1) : (index, index) -> index
    %size_e = "arith.muli"(%e0, %e1) : (index, index) -> index
    %size_f = "arith.muli"(%f0, %f1) : (index, index) -> index
    %s0 = "arith.addi"(%size_a, %size_b) : (index, index) -> index
    %s1 = "arith.addi"(%s0, %size_c) : (index, index) -> index
    %s2 = "arith.addi"(%s1, %size_d) : (index, index) -> index
    %s3 = "arith.addi"(%s2, %size_e) : (index, index) -> index
    %s4 = "arith.addi"(%s3, %size_f) : (index, index) -> index
    func.return %s4 : index
  }
}
