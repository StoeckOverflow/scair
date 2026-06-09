// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %four = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %two = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
    %zero_i = "arith.constant"() <{value = 0 : index}> : () -> index
    %one_i = "arith.constant"() <{value = 1 : index}> : () -> index
    %two_i = "arith.constant"() <{value = 2 : index}> : () -> index
    %buf = d_memref.alloc : () -> !d_memref.memref<[%four], i32>
    %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
    d_memref.store %v, %buf[%two_i] : i32, !d_memref.memref<[%four], i32>
    %sv = d_memref.subview %buf[%zero_i][%two][%two_i] : !d_memref.memref<[%four], i32> -> !d_memref.memref<[%two], i32>
    %r = d_memref.load %sv[%one_i] : !d_memref.memref<[%two], i32> -> i32
    func.return %r : i32
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> i32 {
// IR: %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// IR: %1 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// IR: %[[ZERO:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// IR: %[[ONE:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// IR: %{{[0-9]+}} = "arith.constant"() <{value = 2 : index}> : () -> index
// IR: %[[BUF:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%0], i32>
// IR: %[[V:[0-9]+]] = "arith.constant"() <{value = 7 : i32}> : () -> i32
// IR: d_memref.store %[[V]], %[[BUF]][%{{[0-9]+}}] : i32, !d_memref.memref<[%0], i32>
// IR: %[[SV:[0-9]+]] = d_memref.subview %[[BUF]][%[[ZERO]]][%1][%{{[0-9]+}}] : !d_memref.memref<[%0], i32> -> !d_memref.memref<[%1], i32>
// IR: %[[R:[0-9]+]] = d_memref.load %[[SV]][%[[ONE]]] : !d_memref.memref<[%1], i32> -> i32
// IR: func.return %[[R]] : i32

// EXEC: Result: 7
