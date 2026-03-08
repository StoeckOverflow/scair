// RUN: scair-run %s | filecheck %s

builtin.module {
  func.func @main() -> i32 {
    %n = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %o_i = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = d_memref.alloc : () -> !d_memref.memref<[%n], i32>
    %v = "arith.constant"() <{value = 42 : i32}> : () -> i32
    d_memref.store %v, %m[%o_i] : i32, !d_memref.memref<[%n], i32>
    %r = d_memref.load %m[%o_i] : !d_memref.memref<[%n], i32> -> i32
    d_memref.dealloc %m : !d_memref.memref<[%n], i32>
    func.return %r : i32
  }
}

// CHECK: Result: 42
