// RUN: scair-opt %s --split-input-file -p convert-arith-to-llvm | filecheck %s -DFILE=%s

builtin.module {
  func.func @plain(%a: i32, %b: i32, %x: f32, %y: f32) -> (i32, i32, f32) {
    %c2 = "arith.constant"() <{value = 2 : i32}> : () -> i32
    %sum = "arith.addi"(%a, %b) : (i32, i32) -> i32
    %prod = "arith.muli"(%sum, %c2) : (i32, i32) -> i32
    %min = "arith.minsi"(%prod, %b) : (i32, i32) -> i32
    %fsum = "arith.addf"(%x, %y) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %fprod = "arith.mulf"(%fsum, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %prod, %min, %fprod : i32, i32, f32
  }
}

// CHECK: builtin.module {
// CHECK-NEXT:   func.func @plain(%0: i32, %1: i32, %2: f32, %3: f32) -> (i32, i32, f32) {
// CHECK-NEXT:     %4 = "llvm.mlir.constant"() <{value = 2 : i32}> : () -> i32
// CHECK-NEXT:     %5 = "llvm.add"(%0, %1) : (i32, i32) -> i32
// CHECK-NEXT:     %6 = "llvm.mul"(%5, %4) : (i32, i32) -> i32
// CHECK-NEXT:     %7 = llvm.icmp "slt" %6, %1 : i32
// CHECK-NEXT:     %8 = "llvm.select"(%7, %6, %1) : (i1, i32, i32) -> i32
// CHECK-NEXT:     %9 = "llvm.fadd"(%2, %3) : (f32, f32) -> f32
// CHECK-NEXT:     %10 = "llvm.fmul"(%9, %2) : (f32, f32) -> f32
// CHECK-NEXT:     func.return %6, %8, %10 : i32, i32, f32
// CHECK-NEXT:   }
// CHECK-NEXT: }
