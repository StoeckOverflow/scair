// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | filecheck %s

builtin.module {
  func.func @apply_and_single_expr_min_lower(%a: index, %b: index) -> (index, index) {
    %applied = d_affine.apply affine_map<(d0)[s0] -> (d0 + 2 * s0)>(%a)[%b] : (index)[index] -> index
    %minned = d_affine.min affine_map<(d0)[s0] -> (d0 - s0)>(%a)[%b] : (index)[index] -> index
    "func.return"(%applied, %minned) : (index, index) -> ()
  }
}

// CHECK-LABEL: func.func @apply_and_single_expr_min_lower
// CHECK: "llvm.mul"
// CHECK: "llvm.add"
// CHECK: "llvm.mul"
// CHECK: "llvm.add"
// CHECK-NOT: d_affine.apply
// CHECK-NOT: d_affine.min
