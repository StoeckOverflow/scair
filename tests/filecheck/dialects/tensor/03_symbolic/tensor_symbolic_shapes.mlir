// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,CANONF,CANOND,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefixes=CSE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefixes=DCE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,PIPESYM,DIAG

// Symbolic producers from test.index and index arithmetic are valid dim params.
builtin.module {
  %x = "test.index"() : () -> index
  %y = "test.index"() : () -> index
  %s = "arith.addi"(%x, %y) : (index, index) -> index
  %p = "arith.muli"(%s, %x) : (index, index) -> index
  %z = "test.zero"() : () -> f32
  %t0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%s, %p], f32>
  %t1 = "tensor.fill"(%z) : (f32) -> !d_tensor.tensor<[%s, %p], f32>
  %v = "test.vec"() : () -> !d_tensor.vector<%x, f32>
  %m = "test.mat"() : () -> !d_tensor.matrix<%x, %y, f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.index"() : () -> index
// VERIFY:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// VERIFY:   %3 = "arith.muli"(%2, %0) {{.*}} : (index, index) -> index
// VERIFY:   %4 = "test.zero"() : () -> f32
// VERIFY:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %6 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %7 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// VERIFY:   %8 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "test.index"() : () -> index
// CANON:   %1 = "test.index"() : () -> index
// CANON:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CANON:   %3 = "arith.muli"(%2, %0) {{.*}} : (index, index) -> index
// CANON:   %4 = "test.zero"() : () -> f32
// CANON:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %3], f32>
// CANON:   %6 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// CANON:   %7 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// CANON:   %8 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CANON: }
// CSE: builtin.module {
// CSE:   %0 = "test.index"() : () -> index
// CSE:   %1 = "test.index"() : () -> index
// CSE:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CSE:   %3 = "arith.muli"(%2, %0) {{.*}} : (index, index) -> index
// CSE:   %4 = "test.zero"() : () -> f32
// CSE:   %5 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %3], f32>
// CSE:   %6 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// CSE:   %7 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// CSE:   %8 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "test.index"() : () -> index
// DCE:   %1 = "test.index"() : () -> index
// DCE:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// DCE:   %3 = "arith.muli"(%2, %0) {{.*}} : (index, index) -> index
// DCE: }
// PIPE: builtin.module {
// PIPE:   %0 = "test.index"() : () -> index
// PIPE:   %1 = "test.index"() : () -> index
// PIPE:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// PIPE:   %3 = "arith.muli"(%2, %0) {{.*}} : (index, index) -> index
// PIPE:   %4 = "test.zero"() : () -> f32
// PIPE:   %5 = "tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%2, %3], f32>
// PIPE:   %6 = "test.vec"() : () -> !d_tensor.vector<%0, f32>
// PIPE:   %7 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// PIPE: }

// -----

// Semantically equal but SSA-distinct dims are rejected without canonicalization.
builtin.module {
  %a = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %b = "arith.addi"(%a, %z) : (index, index) -> index
  %c = "arith.addi"(%z, %a) : (index, index) -> index
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
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index

  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %ok = "d_tensor.matmul"(%A, %B)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// VERIFY: // -----
// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.index"() : () -> index
// VERIFY:   %2 = "test.index"() : () -> index
// VERIFY:   %3 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %4 = "test.B"() : () -> !d_tensor.tensor<[%1, %2], f32>
// VERIFY:   %5 = "d_tensor.matmul"(%3, %4) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// VERIFY: }

// -----

// Dedicated symbolic matmul coverage: invalid inner-dim identity mismatch.
builtin.module {
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index

  %k2 = "test.index"() : () -> index
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %Bbad = "test.Bbad"() : () -> !d_tensor.tensor<[%k2, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected SSA-identical inner dims}}
  %bad = "d_tensor.matmul"(%A, %Bbad)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k2, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected SSA-identical inner dims

// -----

// Shape canonicalization preserves index arithmetic dims in Phase 1.
builtin.module {
  %x = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %s = "arith.addi"(%x, %z) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CANONF: builtin.module {
// CANONF:   %0 = "test.index"() : () -> index
// CANONF:   %1 = "arith.constant"() <{value = 0 : index}> : () -> index
// CANONF:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CANONF:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CANONF: }

// -----

// d_tensor.dim extraction chain on symbolic dims remains valid with !value<...> result typing.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// CANOND: builtin.module {
// CANOND:   %0 = "test.index"() : () -> index
// CANOND:   %1 = "test.index"() : () -> index
// CANOND:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CANOND:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// CANOND:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// CANOND: }

// -----

// Pipeline on symbolic dims should preserve validity and reduce redundant index arithmetic.
builtin.module {
  %x = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %s0 = "arith.addi"(%x, %z) : (index, index) -> index
  %s1 = "arith.addi"(%x, %z) : (index, index) -> index
  %u0 = "test.keep"() : () -> !d_tensor.tensor<[%s0], f32>
  %u1 = "test.keep"() : () -> !d_tensor.tensor<[%s1], f32>
}

// PIPESYM: builtin.module {
// PIPESYM:   %0 = "test.index"() : () -> index
// PIPESYM:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPESYM:   %2 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPESYM: }
