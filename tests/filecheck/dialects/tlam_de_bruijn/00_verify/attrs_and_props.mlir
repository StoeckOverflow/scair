// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Purpose: local global verifier coverage for DBI types in generic metadata.

// Valid: metadata inside a TLambda body sees the TLambda-introduced DBI depth.
builtin.module {
  %f = "tlam_dbi.tlambda"() ({
    "test.use"() <{prop = !tlam_dbi.bvar<0>}> {attr = !tlam_dbi.forall<!tlam_dbi.bvar<1>>} : () -> ()
    %v = "test.value"() : () -> i32
    "tlam_dbi.treturn"(%v) : (i32) -> ()
  }) : () -> !tlam_dbi.forall<i32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     "test.use"() <{prop = !tlam_dbi.bvar<0>}> {attr = !tlam_dbi.forall<!tlam_dbi.bvar<1>>} : () -> ()
// CHECK:     %1 = "test.value"() : () -> i32
// CHECK:     "tlam_dbi.treturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<i32>
// CHECK: }

// -----

// Invalid: top-level generic attribute contains an out-of-scope DBI type.
builtin.module {
  "test.use"() {attr = !tlam_dbi.bvar<0>} : () -> ()
}

// CHECK: debruijn-dbi: bvar<0> out of scope at depth=0

// -----

// Invalid: top-level generic property contains an out-of-scope DBI type.
builtin.module {
  "test.use"() <{prop = !tlam_dbi.forall<!tlam_dbi.bvar<1>>}> : () -> ()
}

// CHECK: debruijn-dbi: bvar<1> out of scope at depth=1

// -----

// Valid: unrelated non-DBI metadata is ignored by the DBI verifier.
builtin.module {
  "test.use"() <{prop = !d_tensor.tensor<[], i32>}> {attr = !d_tensor.nat} : () -> ()
}

// CHECK: builtin.module {
// CHECK:   "test.use"() <{prop = !d_tensor.tensor<[], i32>}> {attr = !d_tensor.nat} : () -> ()
// CHECK: }
