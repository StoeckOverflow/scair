// RUN: scair-opt --allow-unregistered-dialect --split-input-file --parsing-diagnostics %s | filecheck %s -DFILE=%s

// Valid: SSA shape param is defined earlier.
builtin.module {
  %n = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %v = "test.ok"() : () -> !dtensor.vector<%n, f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
// CHECK:   %1 = "test.ok"() : () -> !dtensor.vector<%0, f32>
// CHECK: }

// -----

// Invalid: SSA shape param is not defined yet.
builtin.module {
  %v = "test.bad"() : () -> !dtensor.vector<%n, f32>
}

// CHECK: // -----
// CHECK: Parse error at /home/dominic/scair/tests/filecheck/dialects/tensor/01_parse_print/shape_param_parsing.mlir:25:1:
// CHECK: ^
// CHECK: Value %n not defined within Scope
