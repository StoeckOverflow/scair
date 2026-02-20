// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p check-post-erase-tlam | filecheck %s --check-prefix=ERR -DFILE=%s

// Negative (diagnostic style): invalid TLambda shape is reported with expected-error.
builtin.module {
  // expected-error @below {{tlambda: must have exactly one block with zero args}}
  %poly = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}

// ERR: tlambda:
