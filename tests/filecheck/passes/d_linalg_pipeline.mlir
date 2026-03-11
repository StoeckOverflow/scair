// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p lower-dtensor-to-d-linalg,dtensor-to-dmemref-shape-preserving,bufferize-d-linalg-to-dmemref,reconcile-unrealized-casts,lower-d-linalg-to-d-affine,d-affine-to-scf | filecheck %s -DFILE=%s --check-prefix=PIPE

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %k], i32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%k, %n], i32>
  %c = "dtensor.matmul"(%a, %b) : (!dtensor.tensor<[%m, %k], i32>, !dtensor.tensor<[%k, %n], i32>) -> !dtensor.tensor<[%m, %n], i32>
  "test.keep"(%c) : (!dtensor.tensor<[%m, %n], i32>) -> ()
}

// PIPE-LABEL: builtin.module {
// PIPE: %0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// PIPE: %1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// PIPE: %2 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// PIPE: %3 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// PIPE: %4 = d_memref.alloc : () -> !d_memref.memref<[%1, %2], i32>
// PIPE: %5 = "arith.constant"() <{value = 0 : i32}> : () -> i32
// PIPE: %6 = d_memref.alloc : () -> !d_memref.memref<[%0, %2], i32>
// PIPE: %7 = "arith.constant"() <{value = 0 : index}> : () -> index
// PIPE: %8 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// PIPE: %9 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// PIPE: %10 = "arith.constant"() <{value = 1 : index}> : () -> index
// PIPE: "scf.for"(%7, %8, %10) ({
// PIPE: ^bb0(%11: index):
// PIPE:   %12 = "arith.constant"() <{value = 1 : index}> : () -> index
// PIPE:   "scf.for"(%7, %9, %12) ({
// PIPE:   ^bb1(%13: index):
// PIPE:     d_memref.store %5, %6[%11, %13] : i32, !d_memref.memref<[%0, %2], i32>
// PIPE:     "scf.yield"() : () -> ()
// PIPE:   }) : (index, index, index) -> ()
// PIPE:   "scf.yield"() : () -> ()
// PIPE: }) : (index, index, index) -> ()
// PIPE: %11 = "arith.constant"() <{value = 0 : index}> : () -> index
// PIPE: %12 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// PIPE: %13 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// PIPE: %14 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// PIPE: %15 = "arith.constant"() <{value = 1 : index}> : () -> index
// PIPE: "scf.for"(%11, %12, %15) ({
// PIPE: ^bb0(%16: index):
// PIPE:   %17 = "arith.constant"() <{value = 1 : index}> : () -> index
// PIPE:   "scf.for"(%11, %13, %17) ({
// PIPE:   ^bb1(%18: index):
// PIPE:     %19 = "arith.constant"() <{value = 1 : index}> : () -> index
// PIPE:     "scf.for"(%11, %14, %19) ({
// PIPE:     ^bb2(%20: index):
// PIPE:       %21 = d_memref.load %3[%16, %20] : !d_memref.memref<[%0, %1], i32> -> i32
// PIPE:       %22 = d_memref.load %4[%20, %18] : !d_memref.memref<[%1, %2], i32> -> i32
// PIPE:       %23 = d_memref.load %6[%16, %18] : !d_memref.memref<[%0, %2], i32> -> i32
// PIPE:       %24 = "arith.muli"(%21, %22) : (i32, i32) -> i32
// PIPE:       %25 = "arith.addi"(%23, %24) : (i32, i32) -> i32
// PIPE:       d_memref.store %25, %6[%16, %18] : i32, !d_memref.memref<[%0, %2], i32>
// PIPE:       "scf.yield"() : () -> ()
// PIPE:     }) : (index, index, index) -> ()
// PIPE:     "scf.yield"() : () -> ()
// PIPE:   }) : (index, index, index) -> ()
// PIPE:   "scf.yield"() : () -> ()
// PIPE: }) : (index, index, index) -> ()
// PIPE: %16 = "builtin.unrealized_conversion_cast"(%6) : (!d_memref.memref<[%0, %2], i32>) -> !dtensor.tensor<[%0, %2], i32>
// PIPE: "test.keep"(%16) : (!dtensor.tensor<[%0, %2], i32>) -> ()
// PIPE-NOT: "dtensor.matmul"
// PIPE-NOT: "d_linalg.matmul"
// PIPE-NOT: d_affine.for
