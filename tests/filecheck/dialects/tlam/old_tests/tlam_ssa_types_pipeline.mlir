// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize,erase-tlam --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=ERASE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=FULL
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETAFULL
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETALATE

// Targets: end-to-end SSA-in-types TLam pipeline safety, including verifier-fail
// behavior on invalid input and pass-order regressions with beta-reduction.

// Positive: polymorphic identity through monomorphize/erase/lower/full-pipeline.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %id_i64 = "tlam.tapply"(%mk) <{tyArg = i64}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<i64, i64>
}

// MONO-LABEL: builtin.module {
// MONO-NOT: "tlam.tapply"
// MONO: "tlam.vlambda"()
// MONO: }

// ERASE-LABEL: builtin.module {
// ERASE-NOT: "tlam.tlambda"
// ERASE-NOT: "tlam.tapply"
// ERASE-NOT: "tlam.treturn"
// ERASE: "tlam.vlambda"()
// ERASE: }

// LOWER-LABEL: builtin.module {
// LOWER-NOT: "tlam."
// LOWER-DAG: %{{[0-9]+}} = func.constant @lifted_{{[0-9]+}} : (i64) -> i64
// LOWER-DAG: func.func @lifted_{{[0-9]+}}(%{{[0-9]+}}: i64) -> i64 {
// LOWER-DAG: func.return %{{[0-9]+}} : i64
// LOWER: }

// FULL-LABEL: builtin.module {
// FULL-NOT: "tlam."
// FULL: func.func @lifted_{{[0-9]+}}(%{{[0-9]+}}: i64) -> i64 {
// FULL: func.return %{{[0-9]+}} : i64
// FULL: }

// -----

// Negative pipeline: non-dominating tvar in tapply tyArg (must report, not crash).
builtin.module {
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "builtin.unrealized_conversion_cast"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !tlam.tvar<%T>}>
       : (!tlam.forall<i64>) -> i64

  %T = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
}

// MONO: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`
// ERASE: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`
// LOWER: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`
// FULL: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`

// -----

// Negative pipeline: invalid DBI scoping (must report, not crash).
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.bvar<0>):
    "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
  }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
}

// MONO: debruijn: bvar<0> out of scope at depth=0
// ERASE: debruijn: bvar<0> out of scope at depth=0
// LOWER: debruijn: bvar<0> out of scope at depth=0
// FULL: debruijn: bvar<0> out of scope at depth=0

// -----

// Pipeline with beta-reduce integrated before canonicalize.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %r = "tlam.vapply"(%id, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// BETAFULL-LABEL: builtin.module {
// BETAFULL-NOT: "tlam."
// BETAFULL: func.func @lifted_{{[0-9]+}}(%{{[0-9]+}}: i32) -> i32 {
// BETAFULL: func.return %{{[0-9]+}} : i32
// BETAFULL: }

// BETALATE-LABEL: builtin.module {
// BETALATE-NOT: "tlam."
// BETALATE: func.func @lifted_{{[0-9]+}}(%{{[0-9]+}}: i32) -> i32 {
// BETALATE: func.return %{{[0-9]+}} : i32
// BETALATE: }
