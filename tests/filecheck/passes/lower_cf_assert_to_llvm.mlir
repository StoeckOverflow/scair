// RUN: scair-opt %s --allow-unregistered-dialect -p lower-cf-assert-to-llvm | filecheck %s

builtin.module {
  func.func @assert_to_abort(%ok: i1) {
    "cf.assert"(%ok) <{msg = "expected positive"}> : (i1) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @assert_to_abort
// CHECK-SAME: (%[[OK:[0-9]+]]: i1)
// CHECK: "llvm.cond_br"(%[[OK]])[^bb[[PASS:[0-9]+]], ^bb[[FAIL:[0-9]+]]] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK: ^bb[[FAIL]]:
// CHECK: llvm.call @abort() : () -> ()
// CHECK: "llvm.unreachable"() : () -> ()
// CHECK: ^bb[[PASS]]:
// CHECK: func.return
// CHECK-NOT: cf.assert
