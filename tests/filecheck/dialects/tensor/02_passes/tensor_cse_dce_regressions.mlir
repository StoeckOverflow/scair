// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// CSE + deep RAUW: merged dim value must rewrite type-embedded use.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %s0 = "d_tensor.size.add"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s1 = "d_tensor.size.add"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%s1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// CSE:   %1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// CSE:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CSE:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CSE: }

// -----

// DCE removes truly dead nat ops while preserving used dims.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %dead = "d_tensor.size.add"(%m, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.keep"() : () -> !d_tensor.tensor<[%m], f32>
}

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// DCE:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// DCE: }
