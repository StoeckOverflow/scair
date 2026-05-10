// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize | filecheck %s

builtin.module {
  func.func @assert_op(%ok: i1) {
    "cf.assert"(%ok) <{msg = "expected true"}> : (i1) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK: "cf.assert"(%{{[0-9]+}}) <{msg = "expected true"}> : (i1) -> ()
