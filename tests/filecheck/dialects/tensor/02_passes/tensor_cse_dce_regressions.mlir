// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// CSE + deep RAUW: merged dim value must rewrite type-embedded use.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %n = "arith.constant"() <{value = 3 : index}> : () -> index
  %s0 = "arith.addi"(%m, %n) : (index, index) -> index
  %s1 = "arith.addi"(%m, %n) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%s1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "arith.constant"() <{value = 2 : index}> : () -> index
// CSE:   %1 = "arith.constant"() <{value = 3 : index}> : () -> index
// CSE:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CSE:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CSE: }

// -----

// DCE removes truly dead nat ops while preserving used dims.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %dead = "arith.addi"(%m, %z) : (index, index) -> index
  %u = "test.keep"() : () -> !d_tensor.tensor<[%m], f32>
}

// DCE: builtin.module {
// DCE:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
// DCE:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// DCE: }
