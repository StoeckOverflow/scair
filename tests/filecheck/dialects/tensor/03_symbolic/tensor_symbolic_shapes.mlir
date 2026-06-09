// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,CANONF,CANOND,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefixes=CSE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefixes=DCE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,PIPESYM,DIAG

// Symbolic producers from d_tensor.size.param and nat algebra are valid dim params.
builtin.module {
  %x = "d_tensor.size.param"() : () -> !d_tensor.size
  %y = "d_tensor.size.param"() : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%x, %y) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %p = "d_tensor.size.mul"(%s, %x) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %z = "test.zero"() : () -> f32
  %t0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%s, %p], f32>
  %t1 = "tensor.fill"(%z) : (f32) -> !d_tensor.tensor<[%s, %p], f32>
  %v = "test.vec"() : () -> !d_tensor.vector<%x, f32>
  %m = "test.mat"() : () -> !d_tensor.matrix<%x, %y, f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// VERIFY:   %3 = "d_tensor.size.mul"(%2, %0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// VERIFY:   %4 = "test.zero"() : () -> f32
// VERIFY:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %6 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %7 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// VERIFY:   %8 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANON:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANON:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CANON:   %3 = "d_tensor.size.mul"(%2, %0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CANON:   %4 = "test.zero"() : () -> f32
// CANON:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %3], f32>
// CANON:   %6 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// CANON:   %7 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// CANON:   %8 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CANON: }
// CSE: builtin.module {
// CSE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CSE:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CSE:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CSE:   %3 = "d_tensor.size.mul"(%2, %0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CSE:   %4 = "test.zero"() : () -> f32
// CSE:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %3], f32>
// CSE:   %6 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// CSE:   %7 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// CSE:   %8 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// DCE:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// DCE:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// DCE:   %3 = "d_tensor.size.mul"(%2, %0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// DCE: }
// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// PIPE:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// PIPE:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// PIPE:   %3 = "d_tensor.size.mul"(%2, %0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// PIPE:   %4 = "test.zero"() : () -> f32
// PIPE:   %5 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// PIPE:   %6 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// PIPE:   %7 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// PIPE: }

// -----

// Semantically equal but SSA-distinct dims are rejected without canonicalization.
builtin.module {
  %a = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %b = "d_tensor.size.add"(%a, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %c = "d_tensor.size.add"(%z, %a) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %t0 = "test.a"() : () -> !d_tensor.tensor<[%b], f32>
  %t1 = "test.b"() : () -> !d_tensor.tensor<[%c], f32>
  // expected-error @below {{d_tensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "d_tensor.add"(%t0, %t1)
    : (!d_tensor.tensor<[%b], f32>, !d_tensor.tensor<[%c], f32>) -> !d_tensor.tensor<[%b], f32>
}

// DIAG: d_tensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Dedicated symbolic matmul coverage: valid symbolic dims.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %k = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size

  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %ok = "d_tensor.matmul"(%A, %B)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// VERIFY: // -----
// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %3 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %4 = "test.B"() : () -> !d_tensor.tensor<[%1, %2], f32>
// VERIFY:   %5 = "d_tensor.matmul"(%3, %4) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// VERIFY: }

// -----

// Dedicated symbolic matmul coverage: invalid inner-dim identity mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %k = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size

  %k2 = "d_tensor.size.param"() : () -> !d_tensor.size
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %Bbad = "test.Bbad"() : () -> !d_tensor.tensor<[%k2, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected SSA-identical inner dims}}
  %bad = "d_tensor.matmul"(%A, %Bbad)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k2, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected SSA-identical inner dims

// -----

// Shape canonicalization should fold symbolic add(x, 0) and deep-RAUW type-embedded dims.
builtin.module {
  %x = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%x, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CANONF: builtin.module {
// CANONF:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANONF:   %1 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CANONF:   %2 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// CANONF: }

// -----

// d_tensor.dim extraction chain on symbolic dims remains valid with !value<...> result typing.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// CANOND: builtin.module {
// CANOND:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANOND:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANOND:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CANOND:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// CANOND:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// CANOND: }

// -----

// Pipeline on symbolic dims should preserve validity and reduce redundant nat algebra.
builtin.module {
  %x = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %s0 = "d_tensor.size.add"(%x, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s1 = "d_tensor.size.add"(%x, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u0 = "test.keep"() : () -> !d_tensor.tensor<[%s0], f32>
  %u1 = "test.keep"() : () -> !d_tensor.tensor<[%s1], f32>
}

// PIPESYM: builtin.module {
// PIPESYM:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// PIPESYM:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPESYM:   %2 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPESYM: }
