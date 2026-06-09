// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=ELIM
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>

  %a0 = "d_tensor.dim"(%a) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b0 = "d_tensor.dim"(%b) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c0 = "d_tensor.dim"(%c) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "d_tensor.dim"(%a) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b1 = "d_tensor.dim"(%b) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c1 = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>

  %a0_size = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> !d_tensor.size
  %b0_size = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> !d_tensor.size
  %c0_size = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> !d_tensor.size
  %a1_size = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> !d_tensor.size
  %b1_size = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> !d_tensor.size
  %c1_size = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> !d_tensor.size


  %s0 = "d_tensor.size.add"(%a0_size, %b0_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s1 = "d_tensor.size.add"(%s0, %c0_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s2 = "d_tensor.size.add"(%s1, %a1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s3 = "d_tensor.size.add"(%s2, %b1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s4 = "d_tensor.size.add"(%s3, %c1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  "test.keep"(%s4) : (!d_tensor.size) -> ()
}

// ELIM-NOT: "d_tensor.dim"
// ELIM: "builtin.unrealized_conversion_cast"(%0) : (!d_tensor.size) -> !d_tensor.size
// ELIM: "builtin.unrealized_conversion_cast"(%1) : (!d_tensor.size) -> !d_tensor.size

// PIPE-NOT: "d_tensor.dim"
// PIPE-NOT: "builtin.unrealized_conversion_cast"
// PIPE: "test.keep"
