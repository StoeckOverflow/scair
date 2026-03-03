// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: core tensor SSA-shape ops.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%m, %k) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %p = "dtensor.nat.mul"(%s, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %z = "test.zero"() : () -> f32
  %a = "dtensor.fill"(%z) : (f32) -> !dtensor.tensor<[%m, %k], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%k, %n], f32>
  %x = "dtensor.matmul"(%a, %b)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%x) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %v0 = "test.v0"() : () -> !dtensor.vector<%m, f32>
  %m0 = "test.m0"() : () -> !dtensor.matrix<%m, %n, f32>
  %c = "dtensor.cast"(%x)
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// VERIFY:   %3 = "dtensor.nat.add"(%0, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %4 = "dtensor.nat.mul"(%3, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:   %5 = "test.zero"() : () -> f32
// VERIFY:   %6 = "dtensor.fill"(%5) : (f32) -> !dtensor.tensor<[%0, %2], f32>
// VERIFY:   %7 = "dtensor.empty"() : () -> !dtensor.tensor<[%2, %1], f32>
// VERIFY:   %8 = "dtensor.matmul"(%6, %7) : (!dtensor.tensor<[%0, %2], f32>, !dtensor.tensor<[%2, %1], f32>) -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:   %9 = "dtensor.dim"(%8) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:   %10 = "test.v0"() : () -> !dtensor.vector<%0, f32>
// VERIFY:   %11 = "test.m0"() : () -> !dtensor.matrix<%0, %1, f32>
// VERIFY:   %12 = "dtensor.cast"(%8) : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%0, %1], f32>
// VERIFY: }

// -----

// Invalid: dominance-in-types violation with non-dominating dim.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !dtensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: // -----
// VERIFY: ssa-dominance: value Value(!dtensor.nat) does not dominate its use in op `test.use`

// -----

// Parse: vector arity mismatch.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %v = "test.bad"() : () -> !dtensor.vector<%m, %n, f32>
}

// PARSE: // -----
// PARSE: Parse error at /home/dominic/scair/tests/filecheck/dialects/tensor/00_wf/tensor_wf.mlir:66:49:
