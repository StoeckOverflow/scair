// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | filecheck %s -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// Lowering with two value lambdas and chained applies.
builtin.module {
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)

  %g = "tlam_dbi.vlambda"() ({
  ^bb0(%y: i32):
    %r = "tlam_dbi.vapply"(%f, %y) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
    "tlam_dbi.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)

  %c = "arith.constant"() <{value = 4 : i32}> : () -> (i32)
  %z = "tlam_dbi.vapply"(%g, %c) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%z) : (i32) -> ()
}

// CHECK: builtin.module {
// CHECK:   func.func @lifted_2(%0: i32) -> i32 {
// CHECK:     %1 = "func.call"(%0) <{callee = @lifted_1}> : (i32) -> i32
// CHECK:     func.return %1 : i32
// CHECK:   }
// CHECK:   %0 = func.constant @lifted_2 : (i32) -> i32
// CHECK:   func.func @lifted_1(%1: i32) -> i32 {
// CHECK:     func.return %1 : i32
// CHECK:   }
// CHECK:   %1 = func.constant @lifted_1 : (i32) -> i32
// CHECK:   %2 = "arith.constant"() <{value = 4 : i32}> : () -> i32
// CHECK:   %3 = "func.call_indirect"(%0, %2) : ((i32) -> i32, i32) -> i32
// CHECK:   "test.use"(%3) : (i32) -> ()
// CHECK: }
