// RUN: scair-opt %s --allow-unregistered-dialect -p reconcile-unrealized-casts --verify-diagnostics | filecheck %s -DFILE=%s

// Negative: invalid forward use in cast input should diagnose stably.
// expected-error @below {{ssa-dominance: value Value(i64) does not dominate its use in op `builtin.unrealized_conversion_cast`}}
builtin.module {
  func.func @bad() -> i64 {
    %1 = "builtin.unrealized_conversion_cast"(%0) : (i64) -> i64
    %0 = "arith.constant"() <{value = 1 : i64}> : () -> i64
    func.return %1 : i64
  }
}

// CHECK: ssa-dominance: value Value(i64) does not dominate its use in op `builtin.unrealized_conversion_cast`
