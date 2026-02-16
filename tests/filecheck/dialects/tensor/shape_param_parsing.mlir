// RUN: scair-opt --allow-unregistered-dialect --split-input-file --parsing-diagnostics %s | filecheck %s -DFILE=%s

// Valid: SSA shape param is defined earlier.
builtin.module {
  %n = "tensor.nat.const"() <{value = 7 : i32}> : () -> !tensor.nat
  %v = "test.ok"() : () -> !tensor.vector<%n, f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: !tensor.vector<%0, f32>
// CHECK: }

// -----

// Invalid: SSA shape param is not defined yet.
builtin.module {
  %v = "test.bad"() : () -> !tensor.vector<%n, f32>
}

// CHECK: Parse error at [[FILE]]
// CHECK: Value %n must be defined before use in this context.
