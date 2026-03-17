// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p d-affine-min-simplify | filecheck %s -DFILE=%s

builtin.module {
  func.func @min_project_dim(%x: index, %s: index) -> index {
    %m = d_affine.min affine_map<(d0)[s0] -> (d0)>(%x)[%s] : (index)[index] -> index
    func.return %m : index
  }
}
// CHECK-LABEL: func.func @min_project_dim(%0: index, %1: index) -> index {
// CHECK-NEXT:    func.return %0 : index
// CHECK-NEXT:  }

// -----

builtin.module {
  func.func @min_fold_const_expr() -> index {
    %a = "arith.constant"() <{value = 8 : index}> : () -> index
    %b = "arith.constant"() <{value = 4 : index}> : () -> index
    %m = d_affine.min affine_map<(d0)[s0] -> (d0 + s0)>(%a)[%b] : (index)[index] -> index
    func.return %m : index
  }
}
// CHECK-LABEL: func.func @min_fold_const_expr() -> index {
// CHECK-NEXT:    %0 = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK-NEXT:    %1 = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 12 : index}> : () -> index
// CHECK-NEXT:    func.return %2 : index
// CHECK-NEXT:  }

// -----

builtin.module {
  func.func @apply_project_symbol(%d: index, %s: index) -> index {
    %a = d_affine.apply affine_map<(d0)[s0] -> (s0)>(%d)[%s] : (index)[index] -> index
    func.return %a : index
  }
}
// CHECK-LABEL: func.func @apply_project_symbol(%0: index, %1: index) -> index {
// CHECK-NEXT:    func.return %1 : index
// CHECK-NEXT:  }

// -----

builtin.module {
  func.func @apply_fold_const_expr() -> index {
    %a = "arith.constant"() <{value = 5 : index}> : () -> index
    %b = "arith.constant"() <{value = 7 : index}> : () -> index
    %r = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%a)[%b] : (index)[index] -> index
    func.return %r : index
  }
}
// CHECK-LABEL: func.func @apply_fold_const_expr() -> index {
// CHECK-NEXT:    %0 = "arith.constant"() <{value = 5 : index}> : () -> index
// CHECK-NEXT:    %1 = "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 12 : index}> : () -> index
// CHECK-NEXT:    func.return %2 : index
// CHECK-NEXT:  }
