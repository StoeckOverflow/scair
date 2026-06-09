// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %n = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  "test.keep"(%n) : (!d_tensor.size) -> ()
}

// CHECK: "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
