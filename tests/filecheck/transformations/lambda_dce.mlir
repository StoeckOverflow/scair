// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s

// DCE removes unused tlam_dbi lambdas without marking them NoMemoryEffect.
builtin.module {
  %dead_t = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

  %dead_v = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)

  %live_v = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i64):
    "tlam_dbi.vreturn"(%x) : (i64) -> ()
  }) : () -> (!tlam_dbi.fun<i64, i64>)

  "test.use"(%live_v) : (!tlam_dbi.fun<i64, i64>) -> ()
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i64):
// CHECK:     "tlam_dbi.vreturn"(%1) : (i64) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i64, i64>
// CHECK:   "test.use"(%0) : (!tlam_dbi.fun<i64, i64>) -> ()
// CHECK: }

// -----

// DCE removes unused tlam lambdas while preserving used lambdas.
builtin.module {
  %dead_t = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> (!tlam.forall<i64>)

  %dead_v = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)

  %live_v = "tlam.vlambda"() ({
  ^bb0(%x: i64):
    "tlam.vreturn"(%x) : (i64) -> ()
  }) : () -> (!tlam.fun<i64, i64>)

  "test.use"(%live_v) : (!tlam.fun<i64, i64>) -> ()
}
// CHECK: // -----
// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i64):
// CHECK:     "tlam.vreturn"(%1) : (i64) -> ()
// CHECK:   }) : () -> !tlam.fun<i64, i64>
// CHECK:   "test.use"(%0) : (!tlam.fun<i64, i64>) -> ()
// CHECK: }
