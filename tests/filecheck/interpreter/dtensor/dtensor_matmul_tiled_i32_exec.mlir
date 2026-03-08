// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | scair-run | filecheck %s

builtin.module {
  func.func @main() -> i32 {
    %two = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %zero_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %zero = "dtensor.shape.to_index"(%zero_nat) : (!dtensor.nat) -> index
    %one = "arith.constant"() <{value = 1 : index}> : () -> index

    %A = d_memref.alloc : () -> !d_memref.memref<[%two, %two], i32>
    %B = d_memref.alloc : () -> !d_memref.memref<[%two, %two], i32>

    %c1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %c2 = "arith.constant"() <{value = 2 : i32}> : () -> i32
    %c3 = "arith.constant"() <{value = 3 : i32}> : () -> i32
    %c4 = "arith.constant"() <{value = 4 : i32}> : () -> i32
    %c5 = "arith.constant"() <{value = 5 : i32}> : () -> i32
    %c6 = "arith.constant"() <{value = 6 : i32}> : () -> i32
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    %c8 = "arith.constant"() <{value = 8 : i32}> : () -> i32

    d_memref.store %c1, %A[%zero, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c2, %A[%zero, %one] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c3, %A[%one, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c4, %A[%one, %one] : i32, !d_memref.memref<[%two, %two], i32>

    d_memref.store %c5, %B[%zero, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c6, %B[%zero, %one] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c7, %B[%one, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c8, %B[%one, %one] : i32, !d_memref.memref<[%two, %two], i32>

    %At = "builtin.unrealized_conversion_cast"(%A)
      : (!d_memref.memref<[%two, %two], i32>) -> !dtensor.tensor<[%two, %two], i32>
    %Bt = "builtin.unrealized_conversion_cast"(%B)
      : (!d_memref.memref<[%two, %two], i32>) -> !dtensor.tensor<[%two, %two], i32>

    %Ct = "dtensor.matmul"(%At, %Bt)
      : (!dtensor.tensor<[%two, %two], i32>, !dtensor.tensor<[%two, %two], i32>)
      -> !dtensor.tensor<[%two, %two], i32>
    %C = "builtin.unrealized_conversion_cast"(%Ct)
      : (!dtensor.tensor<[%two, %two], i32>) -> !d_memref.memref<[%two, %two], i32>

    %r = d_memref.load %C[%zero, %zero] : !d_memref.memref<[%two, %two], i32> -> i32
    func.return %r : i32
  }
}

// CHECK: Result: 19
