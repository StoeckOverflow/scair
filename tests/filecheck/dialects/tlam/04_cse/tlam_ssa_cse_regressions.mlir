// Purpose: CSE regressions for TLam SSA-in-types identity and region sensitivity.
// Invariants covered: No merge across differing embedded tvar identity; safe merge for equivalent ops.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p cse --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CSE

// Targets: CSE conservativeness around regions and embedded Tvar identity.
// Note: region-bearing ops here are not CSE-eligible (not NoMemoryEffect),
// so this checks they are never merged accidentally.

// 1) Region-bearing ops with different regions must not merge.
builtin.module {
  %a = "scf.execute_region"() ({
  ^bb0:
    %c0 = "arith.constant"() <{value = 0 : i32}> : () -> i32
    "scf.yield"(%c0) : (i32) -> ()
  }) : () -> i32

  %b = "scf.execute_region"() ({
  ^bb0:
    %c1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
    "scf.yield"(%c1) : (i32) -> ()
  }) : () -> i32

  "test.use"(%a, %b) : (i32, i32) -> ()
}

// CSE-LABEL: builtin.module {
// CSE-DAG: %{{[0-9]+}} = "scf.execute_region"()
// CSE-DAG: %{{[0-9]+}} = "scf.execute_region"()
// CSE: "test.use"(%{{[0-9]+}}, %{{[0-9]+}}) : (i32, i32) -> ()
// CSE: }

// -----

// 2) Differing embedded Tvar identity must not merge.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %TA = "test.make_type_a"() : () -> !tlam.type
  %TB = "test.make_type_b"() : () -> !tlam.type

  %a = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !value<%TA>
  %b = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !value<%TB>
  "test.use"(%a, %b) : (!value<%TA>, !value<%TB>) -> ()
}

// CSE-LABEL: builtin.module {
// CSE-DAG: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !value<%{{[0-9]+}}>
// CSE-DAG: [[B:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !value<%{{[0-9]+}}>
// CSE: "test.use"([[A]], [[B]])
// CSE: }

// -----

// 3) Positive control: identical memory-effect-free ops should merge.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %t0 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  %t1 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !value<%t1>} : () -> ()
}

// CSE-LABEL: builtin.module {
// CSE: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CSE: "test.use"() {dep = !value<[[T]]>} : () -> ()
// CSE: }
