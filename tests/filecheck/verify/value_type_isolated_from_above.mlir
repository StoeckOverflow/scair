// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Local values defined inside an isolated function may be referenced from
// value-dependent type information inside that function.
func.func @ok_isolated_local_witness() {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  "test.use"() {dep = !value<%n>} : () -> ()
  func.return
}

// CHECK-LABEL: func.func @ok_isolated_local_witness()
// CHECK: "test.use"() {dep = !value<%{{[0-9]+}}>}

// -----

// An isolated function signature may use an earlier entry argument in a later
// argument type. The witness is explicitly passed through the isolation boundary.
func.func @ok_isolated_signature_explicit_witness(%n : !d_tensor.size,
                                                  %buf : !d_memref.memref<[%n], f32>) {
  func.return
}

// CHECK-LABEL: func.func @ok_isolated_signature_explicit_witness(
// CHECK-SAME: %{{[0-9]+}}: !d_tensor.size
// CHECK-SAME: %{{[0-9]+}}: !d_memref.memref<[%{{[0-9]+}}], f32>

// -----

// Non-isolated nested regions may capture dominating values through
// value-dependent type information.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  "test.region"() ({
    "test.use"() {dep = !value<%n>} : () -> ()
  }) : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: "test.region"() ({
// CHECK: "test.use"() {dep = !value<%{{[0-9]+}}>}

// -----

// Hidden attribute/property references may not implicitly capture values across
// an IsolatedFromAbove boundary.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  func.func @bad_isolated_body_hidden_capture() {
    "test.use"() {dep = !value<%n>} : () -> ()
    func.return
  }
}

// CHECK: ssa-dominance: value Value{{.*}} crosses IsolatedFromAbove boundary in value-dependent type reference

// -----

// Result types may not implicitly capture external witnesses across an isolated
// function boundary either.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  func.func @bad_isolated_body_result_type_capture() {
    %t = "test.tensor"() : () -> !d_tensor.tensor<[%n], f32>
    "test.keep"(%t) : (!d_tensor.tensor<[%n], f32>) -> ()
    func.return
  }
}

// CHECK: ssa-dominance: value Value{{.*}} crosses IsolatedFromAbove boundary in value-dependent type reference

// -----

// The referenced %n is an enclosing value, so the function signature would implicitly capture through
// the isolated function body/signature.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  func.func @bad_isolated_signature_outer_witness(%buf : !d_memref.memref<[%n], f32>) {
    func.return
  }
}

// CHECK: ssa-dominance: value Value{{.*}} crosses IsolatedFromAbove boundary in value-dependent type reference
