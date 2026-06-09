// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %a0 = "d_tensor.dim"(%a) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "d_tensor.dim"(%a) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b0 = "d_tensor.dim"(%b) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b1 = "d_tensor.dim"(%b) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c0 = "d_tensor.dim"(%c) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c1 = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %a0_size = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> !d_tensor.size
  %a1_size = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> !d_tensor.size
  %b0_size = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> !d_tensor.size
  %b1_size = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> !d_tensor.size
  %c0_size = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> !d_tensor.size
  %c1_size = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> !d_tensor.size
  %size_a = "d_tensor.size.mul"(%a0_size, %a1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %size_b = "d_tensor.size.mul"(%b0_size, %b1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %size_c = "d_tensor.size.mul"(%c0_size, %c1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %sum0 = "d_tensor.size.add"(%size_a, %size_b) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %sum1 = "d_tensor.size.add"(%sum0, %size_c) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  "test.keep"(%sum1) : (!d_tensor.size) -> ()
}

// CHECK-NOT: "d_tensor.dim"
// CHECK-NOT: "builtin.unrealized_conversion_cast"
// CHECK: "d_tensor.size.mul"
// CHECK-NOT: "d_tensor.size.mul"
// CHECK: "d_tensor.size.add"
// CHECK: "test.keep"
