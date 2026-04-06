// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | filecheck %s

%u = "test.use"() : () -> !value<%x>
%x = "test.make"() : () -> i32

// CHECK: ssa-dominance:
// CHECK-SAME: does not dominate its use in op `test.use`
