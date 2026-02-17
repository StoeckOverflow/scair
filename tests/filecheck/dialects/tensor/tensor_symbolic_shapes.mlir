// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,CANONF,CANOND
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,PIPESYM

// Symbolic producers from unregistered ops and nat algebra are valid dim params.
builtin.module {
  %x = "test.nat"() : () -> !tensor.nat
  %y = "test.nat"() : () -> !tensor.nat
  %s = "tensor.nat.add"(%x, %y) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %p = "tensor.nat.mul"(%s, %x) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %z = "test.zero"() : () -> f32
  %t0 = "tensor.empty"() : () -> !tensor.tensor<[%s, %p], f32>
  %t1 = "tensor.fill"(%z) : (f32) -> !tensor.tensor<[%s, %p], f32>
  %v = "test.vec"() : () -> !tensor.vector<%x, f32>
  %m = "test.mat"() : () -> !tensor.matrix<%x, %y, f32>
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "tensor.nat.add"
// VERIFY: "tensor.nat.mul"
// VERIFY: !tensor.tensor<[%2, %3], f32>
// VERIFY: !tensor.vector<%0, f32>
// VERIFY: !tensor.matrix<%0, %1, f32>
// VERIFY: }
// CANON-LABEL: builtin.module {
// CANON: "tensor.nat.add"
// CANON: "tensor.nat.mul"
// CANON: "tensor.empty"() : () -> !tensor.tensor<[%2, %3], f32>
// CANON: }
// CSE-LABEL: builtin.module {
// CSE: "tensor.empty"() : () -> !tensor.tensor<[%2, %3], f32>
// CSE: "tensor.fill"(%4) : (f32) -> !tensor.tensor<[%2, %3], f32>
// CSE: }
// DCE-LABEL: builtin.module {
// DCE: "tensor.nat.add"
// DCE: "tensor.nat.mul"
// DCE: }
// PIPE-LABEL: builtin.module {
// PIPE: "test.nat"() : () -> !tensor.nat
// PIPE: "test.nat"() : () -> !tensor.nat
// PIPE: "test.vec"() : () -> !tensor.vector<%0, f32>
// PIPE: "test.mat"() : () -> !tensor.matrix<%0, %1, f32>
// PIPE: }

// -----

// Semantically equal but SSA-distinct dims are rejected without canonicalization.
builtin.module {
  %a = "test.nat"() : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %b = "tensor.nat.add"(%a, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %c = "tensor.nat.add"(%z, %a) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %t0 = "test.a"() : () -> !tensor.tensor<[%b], f32>
  %t1 = "test.b"() : () -> !tensor.tensor<[%c], f32>
  // expected-error @below {{tensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "tensor.add"(%t0, %t1)
    : (!tensor.tensor<[%b], f32>, !tensor.tensor<[%c], f32>) -> !tensor.tensor<[%b], f32>
}

// VERIFY: tensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// DCE must preserve symbolic dim producers used only from type params.
builtin.module {
  %x = "test.nat"() : () -> !tensor.nat
  %t = "tensor.empty"() : () -> !tensor.tensor<[%x], f32>
  "test.keep_tensor"(%t) : (!tensor.tensor<[%x], f32>) -> ()
}

// DCE-LABEL: builtin.module {
// DCE: "test.nat"() : () -> !tensor.nat
// DCE: "tensor.empty"() : () -> !tensor.tensor<[%0], f32>
// DCE: "test.keep_tensor"(%1) : (!tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// CSE must not merge tensor.empty when result types differ by dim SSA identity.
builtin.module {
  %x1 = "test.nat"() : () -> !tensor.nat
  %x2 = "test.nat"() : () -> !tensor.nat
  %t1 = "tensor.empty"() : () -> !tensor.tensor<[%x1], f32>
  %t2 = "tensor.empty"() : () -> !tensor.tensor<[%x2], f32>
}

// CSE-LABEL: builtin.module {
// CSE: "tensor.empty"() : () -> !tensor.tensor<[%0], f32>
// CSE: "tensor.empty"() : () -> !tensor.tensor<[%1], f32>
// CSE: }

// -----

// Shape canonicalization should fold symbolic add(x, 0) and deep-RAUW type-embedded dims.
builtin.module {
  %x = "test.nat"() : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %s = "tensor.nat.add"(%x, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.use"() : () -> !tensor.tensor<[%s], f32>
}

// CANONF: "test.use"() : () -> !tensor.tensor<[%0], f32>

// -----

// tensor.dim extraction chain on symbolic dims folds to exact embedded dim SSA value.
builtin.module {
  %m = "test.nat"() : () -> !tensor.nat
  %n = "test.nat"() : () -> !tensor.nat
  %A = "test.A"() : () -> !tensor.tensor<[%m, %n], f32>
  %d0 = "tensor.dim"(%A) <{axis = 0 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
  %E = "tensor.empty"() : () -> !tensor.tensor<[%d0], f32>
}

// CANOND: "tensor.empty"() : () -> !tensor.tensor<[%0], f32>

// -----

// Pipeline on symbolic dims should preserve validity and reduce redundant nat algebra.
builtin.module {
  %x = "test.nat"() : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %s0 = "tensor.nat.add"(%x, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s1 = "tensor.nat.add"(%x, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u0 = "test.keep"() : () -> !tensor.tensor<[%s0], f32>
  %u1 = "test.keep"() : () -> !tensor.tensor<[%s1], f32>
}

// PIPESYM: "test.nat"() : () -> !tensor.nat
// PIPESYM: "test.keep"() : () -> !tensor.tensor<[%0], f32>
// PIPESYM: "test.keep"() : () -> !tensor.tensor<[%0], f32>
