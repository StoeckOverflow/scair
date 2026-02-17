// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p cse --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,cse,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=FULL

// Targets: SSA-in-types regressions for deep RAUW + CSE and dead-op cleanup.
// Note: this repo has no standalone dce pass; canonicalize performs dead-op removal.

// E1: CSE must NOT merge ops whose result types differ by embedded tvar SSA identity.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %TA = "test.make_type_a"() : () -> !tlam.type
  %TB = "test.make_type_b"() : () -> !tlam.type

  %a = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.tvar<%TA>
  %b = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.tvar<%TB>

  "test.use"(%a, %b) : (!tlam.tvar<%TA>, !tlam.tvar<%TB>) -> ()
}

// CSE-LABEL: builtin.module {
// CSE-DAG: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.tvar<%{{[0-9]+}}>
// CSE-DAG: [[B:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.tvar<%{{[0-9]+}}>
// CSE: "test.use"([[A]], [[B]])
// CSE: }

// FULL-LABEL: builtin.module {
// FULL-DAG: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.tvar<%{{[0-9]+}}>
// FULL-DAG: [[B:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.tvar<%{{[0-9]+}}>
// FULL: "test.use"([[A]], [[B]])
// FULL: }

// -----

// E2: value used only in embedded tvar must not be dead-code-eliminated.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !tlam.forall<!tlam.tvar<%T>>} : () -> ()
}

// DCE-LABEL: builtin.module {
// DCE: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// DCE: "test.use"() {dep = !tlam.forall<!tlam.tvar<[[T]]>>} : () -> ()
// DCE: }

// FULL-LABEL: builtin.module {
// FULL: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// FULL: "test.use"() {dep = !tlam.forall<!tlam.tvar<[[T]]>>} : () -> ()
// FULL: }

// -----

// E3: CSE-triggered RAUW must rewrite embedded tvar references deeply.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T0 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  %T1 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !tlam.tvar<%T1>} : () -> ()
}

// CSE-LABEL: builtin.module {
// CSE: [[T3:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CSE: "test.use"() {dep = !tlam.tvar<[[T3]]>} : () -> ()
// CSE: }

// FULL-LABEL: builtin.module {
// FULL: [[T3:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// FULL: "test.use"() {dep = !tlam.tvar<[[T3]]>} : () -> ()
// FULL: }
