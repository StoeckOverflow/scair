// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Earlier same-signature arguments may be used as value parameters in later
// block argument types.
func.func @ok(%n : index,
              %buf : !d_memref.memref<[%n], f32>) {
  func.return
}

// CHECK-LABEL: func.func @ok(
// CHECK-SAME: %0: index
// CHECK-SAME: %1: !d_memref.memref<[%0], f32>

// -----

// Later same-signature arguments do not dominate earlier argument types.
func.func @bad_later(%buf : !d_memref.memref<[%n], f32>,
                     %n : index) {
  func.return
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate block argument type

// -----

// Body-local operation results do not dominate function-entry argument types.
func.func @bad_body(%buf : !d_memref.memref<[%n], f32>) {
  %n = "arith.constant"() <{value = 7 : index}> : () -> index
  func.return
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate block argument type

// -----

// A block argument type may not reference the argument being defined.
func.func @bad_self(%n : !value<%n>) {
  func.return
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate block argument type

// -----

// The same ordered telescope rule applies to ordinary block labels, not just
// function entry signatures.
"test.region"() ({
^bb0(%n : index, %buf : !d_memref.memref<[%n], f32>):
  "test.ret"() : () -> ()
}) : () -> ()

// CHECK: "test.region"() ({
// CHECK-NEXT: ^bb{{[0-9]+}}(%{{[0-9]+}}: index, %{{[0-9]+}}: !d_memref.memref<[%{{[0-9]+}}], f32>):

// -----

"test.region"() ({
^bb0(%buf : !d_memref.memref<[%n], f32>, %n : index):
  "test.ret"() : () -> ()
}) : () -> ()

// CHECK: ssa-dominance: value Value{{.*}} does not dominate block argument type

// -----

"test.region"() ({
^bb0(%n : !value<%n>):
  "test.ret"() : () -> ()
}) : () -> ()

// CHECK: ssa-dominance: value Value{{.*}} does not dominate block argument type

// -----

"test.region"() ({
^bb0(%buf : !d_memref.memref<[%n], f32>):
  %n = "arith.constant"() <{value = 7 : index}> : () -> index
  "test.ret"() : () -> ()
}) : () -> ()

// CHECK: ssa-dominance: value Value{{.*}} does not dominate block argument type
