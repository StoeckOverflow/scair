// RUN: scair-opt %s | filecheck %s --check-prefix=IR
// RUN: scair-run %s | filecheck %s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %n = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %o_i = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = d_memref.alloc : () -> !d_memref.memref<[%n], i32>
    %v = "arith.constant"() <{value = 42 : i32}> : () -> i32
    d_memref.store %v, %m[%o_i] : i32, !d_memref.memref<[%n], i32>
    %r = d_memref.load %m[%o_i] : !d_memref.memref<[%n], i32> -> i32
    d_memref.dealloc %m : !d_memref.memref<[%n], i32>
    func.return %r : i32
  }
}

// IR-LABEL: builtin.module {
// IR: func.func @main() -> i32 {
// IR: %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// IR: %1 = "arith.constant"() <{value = 1 : index}> : () -> index
// IR: %2 = d_memref.alloc : () -> !d_memref.memref<[%0], i32>
// IR: %3 = "arith.constant"() <{value = 42 : i32}> : () -> i32
// IR: d_memref.store %3, %2[%1] : i32, !d_memref.memref<[%0], i32>
// IR: %4 = d_memref.load %2[%1] : !d_memref.memref<[%0], i32> -> i32
// IR: d_memref.dealloc %2 : !d_memref.memref<[%0], i32>
// IR: func.return %4 : i32

// EXEC: Result: 42
