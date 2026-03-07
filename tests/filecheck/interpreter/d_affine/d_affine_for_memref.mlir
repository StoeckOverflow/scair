// RUN: scair-run %s | filecheck %s

builtin.module {
  func.func @main() -> i32 {
    %lb = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %ub = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
    %st = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
    %one = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
    %m = d_memref.alloc : () -> !d_memref.memref<[%ub], i32>
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_affine.for %iv = %lb to %ub step %st {
      d_memref.store %c7, %m[%iv] : i32, !d_memref.memref<[%ub], i32>
      d_affine.yield
    }
    %r = d_memref.load %m[%one] : !d_memref.memref<[%ub], i32> -> i32
    func.return %r : i32
  }
}

// CHECK: Result: 7
