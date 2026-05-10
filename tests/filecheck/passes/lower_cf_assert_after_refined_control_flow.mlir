// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm,lower-cf-assert-to-llvm | filecheck %s

builtin.module {
  func.func @assert_before_loop(%ok: i1, %lb: index, %ub: index) {
    "cf.assert"(%ok) <{msg = "loop precondition"}> : (i1) -> ()
    d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 {
      "test.keep"(%iv) : (index) -> ()
      d_affine.yield
    }
    func.return
  }
}

// CHECK-LABEL: func.func @assert_before_loop
// CHECK: llvm.cond_br
// CHECK: llvm.call @abort() : () -> ()
// CHECK: llvm.unreachable
// CHECK: llvm.br
// CHECK-NOT: cf.assert
// CHECK-NOT: d_affine.for
