// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid types and add/mul operations.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 8 : index}> : () -> index
  %k = "arith.constant"() <{value = 3 : index}> : () -> index

  %v0 = "test.v0"() : () -> !tensor.vector<%m, f32>
  %v1 = "test.v1"() : () -> !tensor.vector<%m, f32>
  %v2 = "tensor.vadd"(%v0, %v1)
    : (!tensor.vector<%m, f32>, !tensor.vector<%m, f32>) -> !tensor.vector<%m, f32>
  %v3 = "tensor.vmul"(%v0, %v1)
    : (!tensor.vector<%m, f32>, !tensor.vector<%m, f32>) -> !tensor.vector<%m, f32>

  %m0 = "test.m0"() : () -> !tensor.matrix<%m, %n, f32>
  %m1 = "test.m1"() : () -> !tensor.matrix<%m, %n, f32>
  %m2 = "tensor.madd"(%m0, %m1)
    : (!tensor.matrix<%m, %n, f32>, !tensor.matrix<%m, %n, f32>) -> !tensor.matrix<%m, %n, f32>

  %ma = "test.ma"() : () -> !tensor.matrix<%m, %n, f32>
  %mb = "test.mb"() : () -> !tensor.matrix<%n, %k, f32>
  %m3 = "tensor.mmul"(%ma, %mb)
    : (!tensor.matrix<%m, %n, f32>, !tensor.matrix<%n, %k, f32>) -> !tensor.matrix<%m, %k, f32>

  %t0 = "test.t0"() : () -> !tensor.tensor<[2, %k], f32>
  %t1 = "test.t1"() : () -> !tensor.tensor<[2, %k], f32>
  %t2 = "tensor.tadd"(%t0, %t1)
    : (!tensor.tensor<[2, %k], f32>, !tensor.tensor<[2, %k], f32>) -> !tensor.tensor<[2, %k], f32>
  %t3 = "tensor.tmul"(%t0, %t1)
    : (!tensor.tensor<[2, %k], f32>, !tensor.tensor<[2, %k], f32>) -> !tensor.tensor<[2, %k], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tensor.vadd"
// CHECK: "tensor.vmul"
// CHECK: "tensor.madd"
// CHECK: "tensor.mmul"
// CHECK: "tensor.tadd"
// CHECK: "tensor.tmul"
// CHECK: }

// -----

// Invalid shape literal.
builtin.module {
  %v = "test.bad_vec"() : () -> !tensor.vector<-1, f32>
}

// CHECK: shape Nat literal must be >= 0, got -1

// -----

// Invalid shape SSA sort.
builtin.module {
  %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %v = "test.bad_vec"() : () -> !tensor.vector<%x, f32>
}

// CHECK: shape SSA parameter must have type index (or i64), got i32

// -----

// Invalid element type.
builtin.module {
  %k = "arith.constant"() <{value = 5 : index}> : () -> index
  %t = "test.bad_ten"() : () -> !tensor.tensor<[2, %k], tensor<1xf32>>
}

// CHECK: invalid tensor element type

// -----

// Invalid empty tensor rank.
builtin.module {
  %t = "test.empty_rank"() : () -> !tensor.tensor<[], f32>
}

// CHECK: tensor shape rank must be >= 1

// -----

// Invalid op invariant: mismatched result type for vadd.
builtin.module {
  %v0 = "test.v0"() : () -> !tensor.vector<4, f32>
  %v1 = "test.v1"() : () -> !tensor.vector<4, f32>
  %v2 = "tensor.vadd"(%v0, %v1)
    : (!tensor.vector<4, f32>, !tensor.vector<4, f32>) -> !tensor.vector<8, f32>
}

// CHECK: vadd: expected lhs/rhs/res to have the same vector type

// -----

// Invalid mmul: inner dimensions do not match.
builtin.module {
  %a = "test.a"() : () -> !tensor.matrix<2, 3, f32>
  %b = "test.b"() : () -> !tensor.matrix<4, 5, f32>
  %c = "tensor.mmul"(%a, %b)
    : (!tensor.matrix<2, 3, f32>, !tensor.matrix<4, 5, f32>) -> !tensor.matrix<2, 5, f32>
}

// CHECK: mmul: expected (r x k, k x c) -> (r x c) with same element type
