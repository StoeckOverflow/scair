// Purpose: Reconcile unrealized casts regression coverage (positive + negative).
// Invariants covered: reconcile removes reducible chains, keeps live unresolved casts, and does not crash on invalid cast use.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p reconcile-unrealized-casts --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CAST

// Positive: reducible pair is reconciled away.
builtin.module {
  func.func @pair(%arg0: i64) -> i64 {
    %0 = "builtin.unrealized_conversion_cast"(%arg0) : (i64) -> i32
    %1 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> i64
    func.return %1 : i64
  }
}

// CAST-LABEL: func.func @pair
// CAST-NOT: "builtin.unrealized_conversion_cast"
// CAST: func.return %{{[0-9]+}} : i64

// -----

// Positive: unresolved live chain must remain (no silent dropping).
builtin.module {
  func.func @live_chain(%arg0: i64) -> i32 {
    %0 = "builtin.unrealized_conversion_cast"(%arg0) : (i64) -> i1
    %1 = "builtin.unrealized_conversion_cast"(%0) : (i1) -> i32
    func.return %1 : i32
  }
}

// CAST-LABEL: func.func @live_chain
// CAST: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i64) -> i1
// CAST: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i1) -> i32
// CAST: func.return %{{[0-9]+}} : i32

// -----

// Negative: invalid forward use in cast input should diagnose stably.
// expected-error @below {{ssa-dominance: value Value}}
builtin.module {
  func.func @bad() -> i64 {
    %1 = "builtin.unrealized_conversion_cast"(%0) : (i64) -> i64
    %0 = "arith.constant"() <{value = 1 : i64}> : () -> i64
    func.return %1 : i64
  }
}
