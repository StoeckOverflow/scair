// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=ELIM
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim,reconcile-unrealized-casts,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %a = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %c = "test.tensor_arg"() : () -> !d_tensor.tensor<[%m, %n], f32>

  %a0 = "d_tensor.dim"(%a) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %b0 = "d_tensor.dim"(%b) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %c0 = "d_tensor.dim"(%c) <{axis = 0 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %a1 = "d_tensor.dim"(%a) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %b1 = "d_tensor.dim"(%b) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c1 = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>

  %a0_i = "builtin.unrealized_conversion_cast"(%a0) : (!value<%m>) -> index
  %b0_i = "builtin.unrealized_conversion_cast"(%b0) : (!value<%m>) -> index
  %c0_i = "builtin.unrealized_conversion_cast"(%c0) : (!value<%m>) -> index
  %a1_i = "builtin.unrealized_conversion_cast"(%a1) : (!value<%n>) -> index
  %b1_i = "builtin.unrealized_conversion_cast"(%b1) : (!value<%n>) -> index
  %c1_i = "builtin.unrealized_conversion_cast"(%c1) : (!value<%n>) -> index

  %s0 = "arith.addi"(%a0_i, %b0_i) : (index, index) -> index
  %s1 = "arith.addi"(%s0, %c0_i) : (index, index) -> index
  %s2 = "arith.addi"(%s1, %a1_i) : (index, index) -> index
  %s3 = "arith.addi"(%s2, %b1_i) : (index, index) -> index
  %s4 = "arith.addi"(%s3, %c1_i) : (index, index) -> index
  "test.keep"(%s4) : (index) -> ()
}

// ELIM-NOT: "d_tensor.dim"
// ELIM: "builtin.unrealized_conversion_cast"(%0) : (index) -> index
// ELIM: "builtin.unrealized_conversion_cast"(%1) : (index) -> index

// PIPE-NOT: "d_tensor.dim"
// PIPE-NOT: "builtin.unrealized_conversion_cast"
// PIPE-NOT: d_tensor.shape
// PIPE: "arith.addi"
// PIPE: "test.keep"
