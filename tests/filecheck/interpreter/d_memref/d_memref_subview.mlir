// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %four = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %two = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
    %two_i = "d_tensor.shape.to_index"(%two) : (!d_tensor.nat) -> index
    %zero_i = "arith.constant"() <{value = 0 : index}> : () -> index
    %one_i = "arith.constant"() <{value = 1 : index}> : () -> index
    %buf = d_memref.alloc : () -> !d_memref.memref<[%four], i32>
    %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_memref.store %v, %buf[%two_i] : i32, !d_memref.memref<[%four], i32>
    %sv = d_memref.subview %buf[%zero_i][%two_i][%two_i] : !d_memref.memref<[%four], i32> -> !d_memref.memref<[%two], i32>
    %r = d_memref.load %sv[%one_i] : !d_memref.memref<[%two], i32> -> i32
    func.return %r : i32
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> i32 {
// IR: %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// IR: %1 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// IR: %2 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// IR: %3 = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %4 = "arith.constant"() <{value = 1 : index}> : () -> index
// IR: %5 = d_memref.alloc : () -> !d_memref.memref<[%0], i32>
// IR: %6 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// IR: d_memref.store %6, %5[%2] : i32, !d_memref.memref<[%0], i32>
// IR: %7 = d_memref.subview %5[%3][%2][%2] : !d_memref.memref<[%0], i32> -> !d_memref.memref<[%1], i32>
// IR: %8 = d_memref.load %7[%4] : !d_memref.memref<[%1], i32> -> i32
// IR: func.return %8 : i32

// EXEC: Result: 7
