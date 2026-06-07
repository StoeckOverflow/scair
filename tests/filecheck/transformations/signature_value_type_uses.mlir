// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// CSE must RAUW values embedded in block argument types, not only operation
// result types and operation attributes.
builtin.module {
  %n0 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %n1 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  "test.region"() ({
  ^bb0(%buf : !d_memref.memref<[%n1], f32>):
    "test.ret"() : () -> ()
  }) : () -> ()
}

// CSE-LABEL: builtin.module {
// CSE: %[[N:[0-9]+]] = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
// CSE-NOT: "dtensor.nat.const"
// CSE: ^bb{{[0-9]+}}(%{{[0-9]+}}: !d_memref.memref<[%[[N]]], f32>):
// CSE: }

// -----

// DCE must keep a value that is used only from a block argument type.
builtin.module {
  %used = "dtensor.nat.const"() <{value = 9 : i32}> : () -> !dtensor.nat
  %dead = "dtensor.nat.const"() <{value = 10 : i32}> : () -> !dtensor.nat
  "test.region"() ({
  ^bb0(%buf : !d_memref.memref<[%used], f32>):
    "test.ret"() : () -> ()
  }) : () -> ()
}

// DCE-LABEL: builtin.module {
// DCE: %[[USED:[0-9]+]] = "dtensor.nat.const"() <{value = 9 : i32}> : () -> !dtensor.nat
// DCE-NOT: value = 10
// DCE: ^bb{{[0-9]+}}(%{{[0-9]+}}: !d_memref.memref<[%[[USED]]], f32>):
// DCE: }
