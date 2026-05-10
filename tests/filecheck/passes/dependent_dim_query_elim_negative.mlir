// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dependent-dim-query-elim | filecheck %s -DFILE=%s --check-prefix=CHECK

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m], f32>
  %dim = d_memref.dim %buf, %zero : !d_memref.memref<[%m], f32> -> index
  "test.keep"(%dim) : (index) -> ()
}

// CHECK: d_memref.dim
// CHECK-NOT: "dtensor.dim"
