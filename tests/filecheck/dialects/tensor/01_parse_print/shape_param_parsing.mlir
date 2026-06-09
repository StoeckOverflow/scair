// RUN: scair-opt --allow-unregistered-dialect --split-input-file --parsing-diagnostics %s | filecheck %s -DFILE=%s
// RUN: scair-opt --allow-unregistered-dialect --split-input-file --parsing-diagnostics %s | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// Valid: SSA shape param is defined earlier.
builtin.module {
  %n = "arith.constant"() <{value = 7 : index}> : () -> index
  %v = "test.ok"() : () -> !d_tensor.vector<%n, f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK:   %1 = "test.ok"() : () -> !d_tensor.vector<%0, f32>
// CHECK: }
