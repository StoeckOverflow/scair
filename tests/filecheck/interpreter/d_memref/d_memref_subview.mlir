// RUN: scair-run %s | filecheck %s

builtin.module {
  func.func @main() -> i32 {
    %four = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %two = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %two_i = "dtensor.shape.to_index"(%two) : (!dtensor.nat) -> index
    %one_i = "arith.constant"() <{value = 1 : index}> : () -> index
    %buf = d_memref.alloc : () -> !d_memref.memref<[%four], i32>
    %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_memref.store %v, %buf[%two_i] : i32, !d_memref.memref<[%four], i32>
    %sv = d_memref.subview %buf[%one_i][%two_i][%one_i] : !d_memref.memref<[%four], i32> -> !d_memref.memref<[%two], i32>
    %r = d_memref.load %sv[%one_i] : !d_memref.memref<[%two], i32> -> i32
    func.return %r : i32
  }
}

// CHECK: Result: 7
