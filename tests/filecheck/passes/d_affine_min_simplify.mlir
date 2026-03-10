// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p d-affine-min-simplify | filecheck %s -DFILE=%s

builtin.module {
  func.func @min_project_dim(%x: index, %s: index) -> index {
    %m = d_affine.min affine_map<(d0)[s0] -> (d0)>(%x)[%s] : (index)[index] -> index
    func.return %m : index
  }
}
// CHECK: func.func @min_project_dim
// CHECK-NOT: d_affine.min
// CHECK: func.return %{{.*}} : index

// -----

builtin.module {
  func.func @min_fold_const_expr() -> index {
    %a = "arith.constant"() <{value = 8 : index}> : () -> index
    %b = "arith.constant"() <{value = 4 : index}> : () -> index
    %m = d_affine.min affine_map<(d0)[s0] -> (d0 + s0)>(%a)[%b] : (index)[index] -> index
    func.return %m : index
  }
}
// CHECK: "arith.constant"() <{value = 12 : index}> : () -> index
// CHECK-NOT: d_affine.min

// -----

builtin.module {
  func.func @apply_project_symbol(%d: index, %s: index) -> index {
    %a = d_affine.apply affine_map<(d0)[s0] -> (s0)>(%d)[%s] : (index)[index] -> index
    func.return %a : index
  }
}
// CHECK: func.func @apply_project_symbol
// CHECK-NOT: d_affine.apply
// CHECK: func.return %{{.*}} : index

// -----

builtin.module {
  func.func @apply_fold_const_expr() -> index {
    %a = "arith.constant"() <{value = 5 : index}> : () -> index
    %b = "arith.constant"() <{value = 7 : index}> : () -> index
    %r = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%a)[%b] : (index)[index] -> index
    func.return %r : index
  }
}
// CHECK: "arith.constant"() <{value = 12 : index}> : () -> index
// CHECK-NOT: d_affine.apply
