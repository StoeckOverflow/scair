// RUN: scair-opt %s --allow-unregistered-dialect -p lower-cf-assert-to-llvm | filecheck %s

builtin.module {
  func.func @assert_to_abort(%ok: i1) {
    "cf.assert"(%ok) <{msg = "expected positive"}> : (i1) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @assert_to_abort
// CHECK: "llvm.cond_br"(%{{[0-9]+}})[^bb{{[0-9]+}}, ^bb{{[0-9]+}}]
// CHECK: llvm.call @abort() : () -> ()
// CHECK: "llvm.unreachable"() : () -> ()
// CHECK: func.return
// CHECK-NOT: cf.assert
