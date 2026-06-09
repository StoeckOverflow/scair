// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// CSE must RAUW values embedded in block argument types, not only operation
// result types and operation attributes.
builtin.module {
  %n0 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  %n1 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  "test.region"() ({
  ^bb0(%buf : !d_memref.memref<[%n1], f32>):
    "test.ret"() : () -> ()
  }) : () -> ()
}

// CSE-LABEL: builtin.module {
// CSE: %[[N:[0-9]+]] = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// CSE-NOT: "d_tensor.size.constant"
// CSE: ^bb{{[0-9]+}}(%{{[0-9]+}}: !d_memref.memref<[%[[N]]], f32>):
// CSE: }

// -----

// DCE must keep a value that is used only from a block argument type.
builtin.module {
  %used = "d_tensor.size.constant"() <{value = 9 : i32}> : () -> !d_tensor.size
  %dead = "d_tensor.size.constant"() <{value = 10 : i32}> : () -> !d_tensor.size
  "test.region"() ({
  ^bb0(%buf : !d_memref.memref<[%used], f32>):
    "test.ret"() : () -> ()
  }) : () -> ()
}

// DCE-LABEL: builtin.module {
// DCE: %[[USED:[0-9]+]] = "d_tensor.size.constant"() <{value = 9 : i32}> : () -> !d_tensor.size
// DCE-NOT: value = 10
// DCE: ^bb{{[0-9]+}}(%{{[0-9]+}}: !d_memref.memref<[%[[USED]]], f32>):
// DCE: }
