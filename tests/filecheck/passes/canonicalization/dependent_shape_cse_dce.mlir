// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %a = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %a0 = "d_tensor.dim"(%a) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "d_tensor.dim"(%a) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b0 = "d_tensor.dim"(%b) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b1 = "d_tensor.dim"(%b) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c0 = "d_tensor.dim"(%c) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c1 = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %a0_i = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> index
  %a1_i = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> index
  %b0_i = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> index
  %b1_i = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> index
  %c0_i = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> index
  %c1_i = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> index
  %size_a = "arith.muli"(%a0_i, %a1_i) : (index, index) -> index
  %size_b = "arith.muli"(%b0_i, %b1_i) : (index, index) -> index
  %size_c = "arith.muli"(%c0_i, %c1_i) : (index, index) -> index
  %sum0 = "arith.addi"(%size_a, %size_b) : (index, index) -> index
  %sum1 = "arith.addi"(%sum0, %size_c) : (index, index) -> index
  "test.keep"(%sum1) : (index) -> ()
}

// CHECK-NOT: "d_tensor.dim"
// CHECK-NOT: "builtin.unrealized_conversion_cast"
// CHECK-NOT: d_tensor.shape
// CHECK: "arith.muli"
// CHECK-NOT: "arith.muli"
// CHECK: "arith.addi"
// CHECK: "test.keep"
