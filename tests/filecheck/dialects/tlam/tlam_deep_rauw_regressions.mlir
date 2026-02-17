// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p cse --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CSE

// DCE regression (canonicalize has RemoveUnusedOperations):
// %T is used only via tvar embedded in an attribute and must not be removed.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !tlam.tvar<%T>} : () -> ()
}

// CANON-LABEL: builtin.module {
// CANON: %{{[0-9]+}} = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CANON: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CANON: "test.use"() {dep = !tlam.tvar<[[T]]>} : () -> ()
// CANON: }

// -----

// Deep-RAUW regression through CSE.
// CSE merges identical casts and replaceValue must rewrite the tvar reference.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T0 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  %T1 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !tlam.tvar<%T1>} : () -> ()
}

// CSE-LABEL: builtin.module {
// CSE: %{{[0-9]+}} = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CSE: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CSE-NOT: "builtin.unrealized_conversion_cast"
// CSE: "test.use"() {dep = !tlam.tvar<[[T]]>} : () -> ()
// CSE: }

// -----

// CSE must respect embedded tvar SSA identity in result types.
// These are identical except for tvar<%TA> vs tvar<%TB>, so they must not fold.
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
