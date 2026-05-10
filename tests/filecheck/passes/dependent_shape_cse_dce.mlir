// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !dtensor.tensor<[%m, %n], f32>
  %a0 = "dtensor.dim"(%a) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "dtensor.dim"(%a) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b0 = "dtensor.dim"(%b) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b1 = "dtensor.dim"(%b) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c0 = "dtensor.dim"(%c) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c1 = "dtensor.dim"(%c) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %a0_nat = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> !dtensor.nat
  %a1_nat = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> !dtensor.nat
  %b0_nat = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> !dtensor.nat
  %b1_nat = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> !dtensor.nat
  %c0_nat = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> !dtensor.nat
  %c1_nat = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> !dtensor.nat
  %a0_i = "dtensor.shape.to_index"(%a0_nat) : (!dtensor.nat) -> index
  %a1_i = "dtensor.shape.to_index"(%a1_nat) : (!dtensor.nat) -> index
  %b0_i = "dtensor.shape.to_index"(%b0_nat) : (!dtensor.nat) -> index
  %b1_i = "dtensor.shape.to_index"(%b1_nat) : (!dtensor.nat) -> index
  %c0_i = "dtensor.shape.to_index"(%c0_nat) : (!dtensor.nat) -> index
  %c1_i = "dtensor.shape.to_index"(%c1_nat) : (!dtensor.nat) -> index
  %size_a = "arith.muli"(%a0_i, %a1_i) : (index, index) -> index
  %size_b = "arith.muli"(%b0_i, %b1_i) : (index, index) -> index
  %size_c = "arith.muli"(%c0_i, %c1_i) : (index, index) -> index
  %sum0 = "arith.addi"(%size_a, %size_b) : (index, index) -> index
  %sum1 = "arith.addi"(%sum0, %size_c) : (index, index) -> index
  "test.keep"(%sum1) : (index) -> ()
}

// CHECK-NOT: "dtensor.dim"
// CHECK-NOT: "builtin.unrealized_conversion_cast"
// CHECK: "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK: "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NOT: "dtensor.shape.to_index"
// CHECK: "arith.muli"
// CHECK-NOT: "arith.muli"
// CHECK: "arith.addi"
// CHECK: "test.keep"
