// RUN: scair-opt %s --allow-unregistered-dialect | filecheck %s

%x = "test.make"() : () -> i32
%u = "test.use1"() : () -> !value<%x>
%v = "test.use2"() : () -> value<%x>

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.make"() : () -> i32
// CHECK-NEXT:   %1 = "test.use1"() : () -> !value<%0>
// CHECK-NEXT:   %2 = "test.use2"() : () -> !value<%0>
// CHECK-NEXT: }
