// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=ELIM
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>

  %a0 = "dtensor.dim"(%a) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b0 = "dtensor.dim"(%b) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c0 = "dtensor.dim"(%c) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "dtensor.dim"(%a) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b1 = "dtensor.dim"(%b) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c1 = "dtensor.dim"(%c) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>

  %a0_nat = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> !dtensor.nat
  %b0_nat = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> !dtensor.nat
  %c0_nat = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> !dtensor.nat
  %a1_nat = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> !dtensor.nat
  %b1_nat = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> !dtensor.nat
  %c1_nat = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> !dtensor.nat

  %a0_i = "dtensor.shape.to_index"(%a0_nat) : (!dtensor.nat) -> index
  %b0_i = "dtensor.shape.to_index"(%b0_nat) : (!dtensor.nat) -> index
  %c0_i = "dtensor.shape.to_index"(%c0_nat) : (!dtensor.nat) -> index
  %a1_i = "dtensor.shape.to_index"(%a1_nat) : (!dtensor.nat) -> index
  %b1_i = "dtensor.shape.to_index"(%b1_nat) : (!dtensor.nat) -> index
  %c1_i = "dtensor.shape.to_index"(%c1_nat) : (!dtensor.nat) -> index

  %s0 = "arith.addi"(%a0_i, %b0_i) : (index, index) -> index
  %s1 = "arith.addi"(%s0, %c0_i) : (index, index) -> index
  %s2 = "arith.addi"(%s1, %a1_i) : (index, index) -> index
  %s3 = "arith.addi"(%s2, %b1_i) : (index, index) -> index
  %s4 = "arith.addi"(%s3, %c1_i) : (index, index) -> index
  "test.keep"(%s4) : (index) -> ()
}

// ELIM-NOT: "dtensor.dim"
// ELIM: "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
// ELIM: "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat

// PIPE-NOT: "dtensor.dim"
// PIPE-NOT: "builtin.unrealized_conversion_cast"
// PIPE: "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// PIPE: "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// PIPE: "test.keep"
