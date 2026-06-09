builtin.module {
  func.func @dependent_same_shape_different_ssa(
      %m: !d_tensor.nat,
      %n: !d_tensor.nat,
      %a: !d_tensor.tensor<[%m, %n], f32>,
      %b: !d_tensor.tensor<[%m, %n], f32>,
      %c: !d_tensor.tensor<[%m, %n], f32>,
      %d: !d_tensor.tensor<[%m, %n], f32>,
      %e: !d_tensor.tensor<[%m, %n], f32>,
      %f: !d_tensor.tensor<[%m, %n], f32>) -> index {
    %a0 = "d_tensor.dim"(%a) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %b0 = "d_tensor.dim"(%b) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %c0 = "d_tensor.dim"(%c) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %d0 = "d_tensor.dim"(%d) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %e0 = "d_tensor.dim"(%e) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %f0 = "d_tensor.dim"(%f) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
    %a1 = "d_tensor.dim"(%a) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %b1 = "d_tensor.dim"(%b) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %c1 = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %d1 = "d_tensor.dim"(%d) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %e1 = "d_tensor.dim"(%e) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
    %f1 = "d_tensor.dim"(%f) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>

    %a0_nat = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> !d_tensor.nat
    %b0_nat = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> !d_tensor.nat
    %c0_nat = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> !d_tensor.nat
    %d0_nat = "builtin.unrealized_conversion_cast"(%d0) : (!value<%m>) -> !d_tensor.nat
    %e0_nat = "builtin.unrealized_conversion_cast"(%e0) : (!value<%m>) -> !d_tensor.nat
    %f0_nat = "builtin.unrealized_conversion_cast"(%f0) : (!value<%m>) -> !d_tensor.nat
    %a1_nat = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> !d_tensor.nat
    %b1_nat = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> !d_tensor.nat
    %c1_nat = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> !d_tensor.nat
    %d1_nat = "builtin.unrealized_conversion_cast"(%d1) : (!value<%n>) -> !d_tensor.nat
    %e1_nat = "builtin.unrealized_conversion_cast"(%e1) : (!value<%n>) -> !d_tensor.nat
    %f1_nat = "builtin.unrealized_conversion_cast"(%f1) : (!value<%n>) -> !d_tensor.nat

    %a0_i = "d_tensor.shape.to_index"(%a0_nat) : (!d_tensor.nat) -> index
    %b0_i = "d_tensor.shape.to_index"(%b0_nat) : (!d_tensor.nat) -> index
    %c0_i = "d_tensor.shape.to_index"(%c0_nat) : (!d_tensor.nat) -> index
    %d0_i = "d_tensor.shape.to_index"(%d0_nat) : (!d_tensor.nat) -> index
    %e0_i = "d_tensor.shape.to_index"(%e0_nat) : (!d_tensor.nat) -> index
    %f0_i = "d_tensor.shape.to_index"(%f0_nat) : (!d_tensor.nat) -> index
    %a1_i = "d_tensor.shape.to_index"(%a1_nat) : (!d_tensor.nat) -> index
    %b1_i = "d_tensor.shape.to_index"(%b1_nat) : (!d_tensor.nat) -> index
    %c1_i = "d_tensor.shape.to_index"(%c1_nat) : (!d_tensor.nat) -> index
    %d1_i = "d_tensor.shape.to_index"(%d1_nat) : (!d_tensor.nat) -> index
    %e1_i = "d_tensor.shape.to_index"(%e1_nat) : (!d_tensor.nat) -> index
    %f1_i = "d_tensor.shape.to_index"(%f1_nat) : (!d_tensor.nat) -> index

    %size_a = "arith.muli"(%a0_i, %a1_i) : (index, index) -> index
    %size_b = "arith.muli"(%b0_i, %b1_i) : (index, index) -> index
    %size_c = "arith.muli"(%c0_i, %c1_i) : (index, index) -> index
    %size_d = "arith.muli"(%d0_i, %d1_i) : (index, index) -> index
    %size_e = "arith.muli"(%e0_i, %e1_i) : (index, index) -> index
    %size_f = "arith.muli"(%f0_i, %f1_i) : (index, index) -> index
    %s0 = "arith.addi"(%size_a, %size_b) : (index, index) -> index
    %s1 = "arith.addi"(%s0, %size_c) : (index, index) -> index
    %s2 = "arith.addi"(%s1, %size_d) : (index, index) -> index
    %s3 = "arith.addi"(%s2, %size_e) : (index, index) -> index
    %s4 = "arith.addi"(%s3, %size_f) : (index, index) -> index
    func.return %s4 : index
  }
}
