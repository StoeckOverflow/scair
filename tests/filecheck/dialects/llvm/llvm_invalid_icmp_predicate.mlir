// RUN: scair-opt %s --parsing-diagnostics | filecheck %s

%0, %1 = "test.op"() : () -> (i32, i32)
%2 = llvm.icmp "bad" %0, %1 : i32

// CHECK: unknown llvm.icmp predicate 'bad'
