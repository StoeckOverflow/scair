// RUN: scair-opt --allow-unregistered-dialect --split-input-file --parsing-diagnostics %s | filecheck %s -DFILE=%s
// RUN: scair-opt --allow-unregistered-dialect --split-input-file --parsing-diagnostics %s | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// Valid: SSA shape param is defined earlier.
builtin.module {
  %n = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %v = "test.ok"() : () -> !dtensor.vector<%n, f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
// CHECK:   %1 = "test.ok"() : () -> !dtensor.vector<%0, f32>
// CHECK: }
