// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | filecheck %s
// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  func.func @apply_and_single_expr_min_lower(%a: index, %b: index) -> (index, index) {
    %applied = d_affine.apply affine_map<(d0)[s0] -> (d0 + 2 * s0)>(%a)[%b] : (index)[index] -> index
    %minned = d_affine.min affine_map<(d0)[s0] -> (d0 - s0)>(%a)[%b] : (index)[index] -> index
    "func.return"(%applied, %minned) : (index, index) -> ()
  }
}

// CHECK-LABEL: func.func @apply_and_single_expr_min_lower
// CHECK-SAME: %[[A:[0-9]+]]: index, %[[B:[0-9]+]]: index
// CHECK: %[[TWO:[0-9]+]] = "llvm.mlir.constant"() <{value = 2 : index}> : () -> index
// CHECK: %[[TWOB:[0-9]+]] = "llvm.mul"(%[[TWO]], %[[B]]) : (index, index) -> index
// CHECK: %[[APPLIED:[0-9]+]] = "llvm.add"(%[[A]], %[[TWOB]]) : (index, index) -> index
// CHECK: %[[NEGONE:[0-9]+]] = "llvm.mlir.constant"() <{value = -1 : index}> : () -> index
// CHECK: %[[NEGB:[0-9]+]] = "llvm.mul"(%[[B]], %[[NEGONE]]) : (index, index) -> index
// CHECK: %[[MINNED:[0-9]+]] = "llvm.add"(%[[A]], %[[NEGB]]) : (index, index) -> index
// CHECK: func.return %[[APPLIED]], %[[MINNED]] : index, index
// CHECK-NOT: d_affine.apply
// CHECK-NOT: d_affine.min
