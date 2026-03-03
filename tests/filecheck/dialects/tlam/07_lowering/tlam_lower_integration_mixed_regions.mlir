// Purpose: Integration coverage for lowering across nested regions with multiple lambdas.
// Invariants covered: preexisting symbol collision avoidance, hierarchical dominance of lifted constants, and full value-level TLam lowering.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p lower-tlam-to-func --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER

builtin.module {
  // Preexisting symbol that should not be reused by lifting.
  func.func @lifted_1(%x: i32) -> i32 {
    func.return %x : i32
  }

  // Lambda A: used from top-level and from inside a nested region.
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  // Top-level use of lambda A.
  %a = "arith.constant"() <{value = 11 : i32}> : () -> i32
  %r0 = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
  "test.use"(%r0) : (i32) -> ()

  // Nested region: defines lambda B and uses both A and B.
  "scf.execute_region"() ({
  ^bb0:
    %g = "tlam.vlambda"() ({
    ^bb1(%y: i32):
      "tlam.vreturn"(%y) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>

    %b = "arith.constant"() <{value = 22 : i32}> : () -> i32
    %r1 = "tlam.vapply"(%f, %b) : (!tlam.fun<i32, i32>, i32) -> i32
    %r2 = "tlam.vapply"(%g, %b) : (!tlam.fun<i32, i32>, i32) -> i32
    "test.use2"(%r1, %r2) : (i32, i32) -> ()
    "scf.yield"() : () -> ()
  }) : () -> ()
}

// LOWER: builtin.module {
// LOWER:   %0 = func.constant @lifted_3 : (i32) -> i32
// LOWER:   func.func @lifted_3(%1: i32) -> i32 {
// LOWER:     func.return %1 : i32
// LOWER:   }
// LOWER:   %1 = func.constant @lifted_2 : (i32) -> i32
// LOWER:   func.func @lifted_2(%2: i32) -> i32 {
// LOWER:     func.return %2 : i32
// LOWER:   }
// LOWER:   func.func @lifted_1(%2: i32) -> i32 {
// LOWER:     func.return %2 : i32
// LOWER:   }
// LOWER:   %2 = "arith.constant"() <{value = 11 : i32}> : () -> i32
// LOWER:   %3 = "func.call_indirect"(%1, %2) : ((i32) -> i32, i32) -> i32
// LOWER:   "test.use"(%3) : (i32) -> ()
// LOWER:   "scf.execute_region"() ({
// LOWER:     %4 = "arith.constant"() <{value = 22 : i32}> : () -> i32
// LOWER:     %5 = "func.call_indirect"(%1, %4) : ((i32) -> i32, i32) -> i32
// LOWER:     %6 = "func.call_indirect"(%0, %4) : ((i32) -> i32, i32) -> i32
// LOWER:     "test.use2"(%5, %6) : (i32, i32) -> ()
// LOWER:     "scf.yield"() : () -> ()
// LOWER:   }) : () -> ()
// LOWER: }
