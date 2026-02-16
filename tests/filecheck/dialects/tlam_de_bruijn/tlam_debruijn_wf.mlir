// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// VALID: well-scoped bvar under TLambda/ForAll and structural rules.
builtin.module {
  %0 = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
}
// CHECK: builtin.module {
// CHECK: "tlam.tlambda"
// CHECK: "tlam.vlambda"
// CHECK: "tlam.treturn"

// -----

// EXPLICIT POLYMORPHISM 1:
// Valid polymorphic identity value: Λ. (λ(x:#0). x)
builtin.module {
  %poly_id = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
}
// CHECK: %[[POLY:.+]] = "tlam.tlambda"
// CHECK: !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

// -----

// EXPLICIT POLYMORPHISM 2:
// Explicit type application of polymorphic identity at i32.
builtin.module {
  %poly_id = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

  %spec = "tlam.tapply"(%poly_id) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
}
// CHECK: "tlam.tapply"
// CHECK: !tlam.fun<i32, i32>

// -----

// VALID: tapply instantiation result type matches instantiate(forall, arg).
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

  %spec = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
}
// CHECK: "tlam.tapply"
// CHECK: !tlam.fun<i64, i64>

// -----

// VALID: vapply typing succeeds for fun<in,out> and arg:in -> out.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %x = "test.op"() : () -> (i32)
  %r = "tlam.vapply"(%id, %x) : (!tlam.fun<i32, i32>, i32) -> (i32)
}
// CHECK: "tlam.vapply"

// -----

// INVALID: bvar out of bounds (k == depth).
builtin.module {
  %0 = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<1>):
      "tlam.vreturn"(%x) : (!tlam.bvar<1>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>>)
}
// CHECK: debruijn: bvar<1> out of scope at depth=1

// -----

// INVALID: bvar out of bounds (k < 0).
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.bvar<-1>):
    "tlam.vreturn"(%x) : (!tlam.bvar<-1>) -> ()
  }) : () -> (!tlam.fun<!tlam.bvar<-1>, !tlam.bvar<-1>>)
}
// CHECK: debruijn: bvar<-1> out of scope at depth=0

// -----

// INVALID: tapply result type does not match instantiation.
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  %bad = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i32>)
}
// CHECK: tapply: result
// CHECK: instantiated

// -----

// INVALID: vapply with wrong argument type.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %x = "test.op"() : () -> (i64)
  %bad = "tlam.vapply"(%id, %x) : (!tlam.fun<i32, i32>, i64) -> (i32)
}
// CHECK: vapply: expected arg i32 and result i32, got i64 and i32

// -----

// INVALID: vlambda without vreturn terminator.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// CHECK: vlambda: last op must be tlam.vreturn

// -----

// INVALID: tlambda without treturn terminator.
builtin.module {
  %0 = "tlam.tlambda"() ({
    "test.op"() : () -> ()
  }) : () -> (!tlam.forall<i32>)
}
// CHECK: tlambda: last op must be tlam.treturn

// -----

// INVALID: vlambda with wrong number of block args.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32, %y: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// CHECK: vlambda: one block with one arg of input type required

// -----

// INVALID: tlambda with wrong number of block args.
builtin.module {
  %0 = "tlam.tlambda"() ({
  ^bb0(%a: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// CHECK: tlambda: must have exactly one block with zero args

// -----

// INVALID: tlambda with multiple blocks.
builtin.module {
  %0 = "tlam.tlambda"() ({
  ^bb0:
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
  ^bb1:
    %id2 = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id2) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// CHECK: tlambda: must have exactly one block with zero args

// -----

// VALID: nested binders allow bvar<1> under two TLambda binders.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %inner = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<1>):
        "tlam.vreturn"(%x) : (!tlam.bvar<1>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>>)
    "tlam.treturn"(%inner) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>>>)
}
// CHECK: "tlam.tlambda"
// CHECK: !tlam.bvar<1>

// -----

// INVALID: bvar with depth=0 (no enclosing binders).
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.bvar<0>):
    "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
  }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
}
// CHECK: debruijn: bvar<0> out of scope at depth=0

// -----

// INVALID: forall body uses out-of-scope bvar at top-level.
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  %bad = "tlam.tapply"(%poly) <{tyArg = !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>, !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
}
// CHECK: debruijn: bvar<1> out of scope at depth=1

// -----

// INVALID: vlambda with zero block args.
builtin.module {
  %id = "tlam.vlambda"() ({
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// CHECK: vlambda: one block with one arg of input type required

// -----

// INVALID: treturn is not last (extra op after terminator).
builtin.module {
  %0 = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// CHECK: tlambda: last op must be tlam.treturn, got 'test.op'

// -----

// INVALID: vapply with non-function callee type.
builtin.module {
  %x = "test.op"() : () -> (i32)
  %y = "test.op"() : () -> (i32)
  %bad = "tlam.vapply"(%x, %y) : (i32, i32) -> (i32)
}
// CHECK: vapply: expected callee of type tlam.fun<in,out>, got i32

// -----

// INVALID: vapply with result type mismatch annotation.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %x = "test.op"() : () -> (i32)
  %bad = "tlam.vapply"(%id, %x) : (!tlam.fun<i32, i32>, i32) -> (i64)
}
// CHECK: vapply: expected arg i32 and result i32, got i32 and i64

// -----

// INVALID: tapply callee is not forall.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %bad = "tlam.tapply"(%id) <{tyArg = i32}> : (!tlam.fun<i32, i32>) -> (i32)
}
// CHECK: tapply: expected operand of type tlam.forall

// -----

// INVALID: tapply type argument is not a type.
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  %bad = "tlam.tapply"(%poly) <{tyArg = "oops"}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
}
// CHECK: tapply: expected type argument, got "oops"

// -----

// VALID: capture-avoiding instantiation under binders.
builtin.module {
  %f = "tlam.tlambda"() ({
    %g = "tlam.tlambda"() ({
      %h = "tlam.tlambda"() ({
        %u = "test.op"() : () -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>)
        "tlam.treturn"(%u) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
      }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>)
      "tlam.treturn"(%h) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
    }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
    %spec = "tlam.tapply"(%g) <{tyArg = !tlam.bvar<0>}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>) -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>)
    "tlam.treturn"(%spec) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
}
// CHECK: !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
