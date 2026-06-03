// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// VALID: well-scoped bvar under TLambda/ForAll and structural rules.
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%2: !tlam_dbi.bvar<0>):
// CHECK:       "tlam_dbi.vreturn"(%2) : (!tlam_dbi.bvar<0>) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// CHECK: }

// -----

// EXPLICIT POLYMORPHISM 1:
// Valid polymorphic identity value: forall. (lambda(x:#0). x)
builtin.module {
  %poly_id = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
}
// CHECK: // -----
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%2: !tlam_dbi.bvar<0>):
// CHECK:       "tlam_dbi.vreturn"(%2) : (!tlam_dbi.bvar<0>) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// CHECK: }

// -----

// EXPLICIT POLYMORPHISM 2:
// Explicit type application of polymorphic identity at i32.
builtin.module {
  %poly_id = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

  %spec = "tlam_dbi.tapply"(%poly_id) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i32, i32>)
}
// CHECK: // -----
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%2: !tlam_dbi.bvar<0>):
// CHECK:       "tlam_dbi.vreturn"(%2) : (!tlam_dbi.bvar<0>) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// CHECK:   %1 = "tlam_dbi.tapply"(%0) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i32, i32>
// CHECK: }

// -----

// VALID: vapply typing succeeds for fun<in,out> and arg:in -> out.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %x = "test.op"() : () -> (i32)
  %r = "tlam_dbi.vapply"(%id, %x) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
}
// CHECK: // -----
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "test.op"() : () -> i32
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK: }

// -----

// INVALID: bvar out of bounds (k == depth).
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<1>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<1>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>)
}
// CHECK: debruijn-dbi: bvar<1> out of scope at depth=1

// -----

// INVALID: bvar out of bounds (k < 0).
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: !tlam_dbi.bvar<-1>):
    "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<-1>) -> ()
  }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<-1>, !tlam_dbi.bvar<-1>>)
}
// CHECK: debruijn-dbi: bvar<-1> out of scope at depth=0

// -----

// INVALID: tapply result type does not match instantiation.
builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  %bad = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i32>)
}
// CHECK: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>

// -----

// INVALID: vapply with wrong argument type.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %x = "test.op"() : () -> (i64)
  %bad = "tlam_dbi.vapply"(%id, %x) : (!tlam_dbi.fun<i32, i32>, i64) -> (i32)
}
// CHECK: vapply: expected arg i32 and result i32, got i64 and i32

// -----

// INVALID: vlambda without vreturn terminator.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
}
// CHECK: // -----
// CHECK: vlambda: last op must be tlam.vreturn, got 'test.op'

// -----

// INVALID: tlambda without treturn terminator.
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.forall<i32>)
}
// CHECK: tlambda: last op must be tlam.treturn, got 'test.op'

// -----

// INVALID: vlambda with wrong number of block args.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32, %y: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
}
// CHECK: vlambda: one block with one arg of input type required

// -----

// INVALID: tlambda with wrong number of block args.
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
  ^bb0(%a: !tlam_dbi.type):
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<i32, i32>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
}
// CHECK: tlambda: must have exactly one block with zero args

// -----

// INVALID: tlambda with multiple blocks.
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
  ^bb0:
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<i32, i32>) -> ()
  ^bb1:
    %id2 = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id2) : (!tlam_dbi.fun<i32, i32>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
}
// CHECK: tlambda: must have exactly one block with zero args

// -----

// VALID: nested binders allow bvar<1> under two TLambda binders.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %inner = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<1>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<1>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>)
    "tlam_dbi.treturn"(%inner) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.tlambda"() ({
// CHECK:       %2 = "tlam_dbi.vlambda"() ({
// CHECK:       ^bb0(%3: !tlam_dbi.bvar<1>):
// CHECK:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<1>) -> ()
// CHECK:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>
// CHECK:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>) -> ()
// CHECK:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>>
// CHECK: }

// -----

// INVALID: bvar with depth=0 (no enclosing binders).
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: !tlam_dbi.bvar<0>):
    "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
  }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
}
// CHECK: debruijn-dbi: bvar<0> out of scope at depth=0

// -----

// INVALID: forall body uses out-of-scope bvar at top-level.
builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  %bad = "tlam_dbi.tapply"(%poly) <{tyArg = !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>, !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>)
}
// CHECK: debruijn-dbi: bvar<1> out of scope at depth=1

// -----

// INVALID: vlambda with zero block args.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
}
// CHECK: vlambda: one block with one arg of input type required

// -----

// INVALID: treturn is not last (extra op after terminator).
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<i32, i32>) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
}
// CHECK: tlambda: last op must be tlam.treturn, got 'test.op'

// -----

// INVALID: vapply with non-function callee type.
builtin.module {
  %x = "test.op"() : () -> (i32)
  %y = "test.op"() : () -> (i32)
  %bad = "tlam_dbi.vapply"(%x, %y) : (i32, i32) -> (i32)
}
// CHECK: vapply: expected callee of type tlam.fun<in,out>, got i32

// -----

// INVALID: vapply with result type mismatch annotation.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %x = "test.op"() : () -> (i32)
  %bad = "tlam_dbi.vapply"(%id, %x) : (!tlam_dbi.fun<i32, i32>, i32) -> (i64)
}
// CHECK: vapply: expected arg i32 and result i32, got i32 and i64

// -----

// INVALID: tapply callee is not forall.
builtin.module {
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %bad = "tlam_dbi.tapply"(%id) <{tyArg = i32}> : (!tlam_dbi.fun<i32, i32>) -> (i32)
}
// CHECK: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>

// -----

// INVALID: tapply type argument is not a type.
builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  %bad = "tlam_dbi.tapply"(%poly) <{tyArg = "oops"}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i32, i32>)
}
// CHECK: tapply: expected type argument, got "oops"
