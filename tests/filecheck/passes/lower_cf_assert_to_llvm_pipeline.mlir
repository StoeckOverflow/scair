// RUN: scair-opt %s --allow-unregistered-dialect -p lower-cf-assert-to-llvm,convert-func-to-llvm | filecheck %s

builtin.module {
  func.func @assert_then_convert(%ok: i1) {
    "cf.assert"(%ok) <{msg = "expected true"}> : (i1) -> ()
    func.return
  }
}

// CHECK-LABEL: llvm.func @assert_then_convert
// CHECK: llvm.cond_br
// CHECK: llvm.call @abort() : () -> ()
// CHECK: llvm.unreachable
// CHECK: llvm.return
// CHECK-NOT: cf.assert
