// RUN: scair-opt %s -p lower-d-memref-to-llvm | filecheck %s --implicit-check-not=arith.divui --implicit-check-not=arith.remui

builtin.module {
  func.func @conv2d_index_reconstruction(%p: index, %khkw: index, %kw: index) {
    %ci = "arith.divui"(%p, %khkw) : (index, index) -> index
    %filter_p = "arith.remui"(%p, %khkw) : (index, index) -> index
    %kh_idx = "arith.divui"(%filter_p, %kw) : (index, index) -> index
    %kw_idx = "arith.remui"(%filter_p, %kw) : (index, index) -> index
    func.return
  }
}

// CHECK-LABEL: func.func @conv2d_index_reconstruction(%0: i64, %1: i64, %2: i64)
// CHECK: "llvm.udiv"(%{{[0-9]+}}, %{{[0-9]+}}) : (i64, i64) -> i64
// CHECK: "llvm.urem"(%{{[0-9]+}}, %{{[0-9]+}}) : (i64, i64) -> i64
// CHECK: "llvm.udiv"(%{{[0-9]+}}, %{{[0-9]+}}) : (i64, i64) -> i64
// CHECK: "llvm.urem"(%{{[0-9]+}}, %{{[0-9]+}}) : (i64, i64) -> i64
