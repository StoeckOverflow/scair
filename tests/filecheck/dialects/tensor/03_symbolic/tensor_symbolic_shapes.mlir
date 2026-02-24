// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,CANONF,CANOND
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,PIPESYM

// Symbolic producers from dtensor.nat.param and nat algebra are valid dim params.
builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %y = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%x, %y) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %p = "dtensor.nat.mul"(%s, %x) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %z = "test.zero"() : () -> f32
  %t0 = "dtensor.empty"() : () -> !dtensor.tensor<[%s, %p], f32>
  %t1 = "tensor.fill"(%z) : (f32) -> !dtensor.tensor<[%s, %p], f32>
  %v = "test.vec"() : () -> !dtensor.vector<%x, f32>
  %m = "test.mat"() : () -> !dtensor.matrix<%x, %y, f32>
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "dtensor.nat.add"
// VERIFY: "dtensor.nat.mul"
// VERIFY: !dtensor.tensor<[%2, %3], f32>
// VERIFY: !dtensor.vector<%0, f32>
// VERIFY: !dtensor.matrix<%0, %1, f32>
// VERIFY: }
// CANON-LABEL: builtin.module {
// CANON: "dtensor.nat.add"
// CANON: "dtensor.nat.mul"
// CANON: "dtensor.empty"() : () -> !dtensor.tensor<[%2, %3], f32>
// CANON: }
// CSE-LABEL: builtin.module {
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%2, %3], f32>
// CSE: "tensor.fill"(%4) : (f32) -> !dtensor.tensor<[%2, %3], f32>
// CSE: }
// DCE-LABEL: builtin.module {
// DCE: "dtensor.nat.add"
// DCE: "dtensor.nat.mul"
// DCE: }
// PIPE-LABEL: builtin.module {
// PIPE: "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE: "test.vec"() : () -> !dtensor.vector<%0, f32>
// PIPE: "test.mat"() : () -> !dtensor.matrix<%0, %1, f32>
// PIPE: }

// -----

// Semantically equal but SSA-distinct dims are rejected without canonicalization.
builtin.module {
  %a = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %b = "dtensor.nat.add"(%a, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %c = "dtensor.nat.add"(%z, %a) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %t0 = "test.a"() : () -> !dtensor.tensor<[%b], f32>
  %t1 = "test.b"() : () -> !dtensor.tensor<[%c], f32>
  // expected-error @below {{tensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "tensor.add"(%t0, %t1)
    : (!dtensor.tensor<[%b], f32>, !dtensor.tensor<[%c], f32>) -> !dtensor.tensor<[%b], f32>
}

// -----

// Dedicated symbolic matmul coverage: valid symbolic dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat

  %A = "test.A"() : () -> !dtensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !dtensor.tensor<[%k, %n], f32>
  %ok = "dtensor.matmul"(%A, %B)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// VERIFY: "dtensor.matmul"
// VERIFY: !dtensor.tensor<[%0, %2], f32>

// -----

// Dedicated symbolic matmul coverage: invalid inner-dim identity mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat

  %k2 = "dtensor.nat.param"() : () -> !dtensor.nat
  %A = "test.A"() : () -> !dtensor.tensor<[%m, %k], f32>
  %Bbad = "test.Bbad"() : () -> !dtensor.tensor<[%k2, %n], f32>
  // expected-error @below {{dtensor.matmul: expected SSA-identical inner dims}}
  %bad = "dtensor.matmul"(%A, %Bbad)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k2, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// -----

// Shape canonicalization should fold symbolic add(x, 0) and deep-RAUW type-embedded dims.
builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%x, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%s], f32>
}

// CANONF: "test.use"() : () -> !dtensor.tensor<[%0], f32>

// -----

// dtensor.dim extraction chain on symbolic dims remains valid with !value<...> result typing.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %A = "test.A"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%A) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "dtensor.empty"() : () -> !dtensor.tensor<[%d0], f32>
}

// CANOND: "dtensor.dim"
// CANOND: "dtensor.empty"()

// -----

// Pipeline on symbolic dims should preserve validity and reduce redundant nat algebra.
builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%x, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%x, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u0 = "test.keep"() : () -> !dtensor.tensor<[%s0], f32>
  %u1 = "test.keep"() : () -> !dtensor.tensor<[%s1], f32>
}

// PIPESYM: "dtensor.nat.param"() : () -> !dtensor.nat
// PIPESYM: "test.keep"() : () -> !dtensor.tensor<[%0], f32>
// PIPESYM: "test.keep"() : () -> !dtensor.tensor<[%0], f32>
