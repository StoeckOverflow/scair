builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %e = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %f = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>

  %a0 = "dtensor.dim"(%a) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b0 = "dtensor.dim"(%b) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c0 = "dtensor.dim"(%c) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %d0 = "dtensor.dim"(%d) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %e0 = "dtensor.dim"(%e) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %f0 = "dtensor.dim"(%f) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "dtensor.dim"(%a) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b1 = "dtensor.dim"(%b) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c1 = "dtensor.dim"(%c) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %d1 = "dtensor.dim"(%d) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %e1 = "dtensor.dim"(%e) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %f1 = "dtensor.dim"(%f) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>

  %a0_nat = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> !dtensor.nat
  %b0_nat = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> !dtensor.nat
  %c0_nat = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> !dtensor.nat
  %d0_nat = "builtin.unrealized_conversion_cast"(%d0) : (!value<%m>) -> !dtensor.nat
  %e0_nat = "builtin.unrealized_conversion_cast"(%e0) : (!value<%m>) -> !dtensor.nat
  %f0_nat = "builtin.unrealized_conversion_cast"(%f0) : (!value<%m>) -> !dtensor.nat
  %a1_nat = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> !dtensor.nat
  %b1_nat = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> !dtensor.nat
  %c1_nat = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> !dtensor.nat
  %d1_nat = "builtin.unrealized_conversion_cast"(%d1) : (!value<%n>) -> !dtensor.nat
  %e1_nat = "builtin.unrealized_conversion_cast"(%e1) : (!value<%n>) -> !dtensor.nat
  %f1_nat = "builtin.unrealized_conversion_cast"(%f1) : (!value<%n>) -> !dtensor.nat

  %a0_i = "dtensor.shape.to_index"(%a0_nat) : (!dtensor.nat) -> index
  %b0_i = "dtensor.shape.to_index"(%b0_nat) : (!dtensor.nat) -> index
  %c0_i = "dtensor.shape.to_index"(%c0_nat) : (!dtensor.nat) -> index
  %d0_i = "dtensor.shape.to_index"(%d0_nat) : (!dtensor.nat) -> index
  %e0_i = "dtensor.shape.to_index"(%e0_nat) : (!dtensor.nat) -> index
  %f0_i = "dtensor.shape.to_index"(%f0_nat) : (!dtensor.nat) -> index
  %a1_i = "dtensor.shape.to_index"(%a1_nat) : (!dtensor.nat) -> index
  %b1_i = "dtensor.shape.to_index"(%b1_nat) : (!dtensor.nat) -> index
  %c1_i = "dtensor.shape.to_index"(%c1_nat) : (!dtensor.nat) -> index
  %d1_i = "dtensor.shape.to_index"(%d1_nat) : (!dtensor.nat) -> index
  %e1_i = "dtensor.shape.to_index"(%e1_nat) : (!dtensor.nat) -> index
  %f1_i = "dtensor.shape.to_index"(%f1_nat) : (!dtensor.nat) -> index

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
  "test.keep"(%s4) : (index) -> ()
}
