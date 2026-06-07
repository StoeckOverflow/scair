// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | filecheck %s
// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  func.func @shifted_bound_lowers_directly(%ub: index, %out: memref<?xf32>) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0 + 4)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @shifted_bound_lowers_directly
// CHECK-SAME: (%[[UB:[0-9]+]]: index, %[[OUT:[0-9]+]]: memref<?xf32>)
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[F0:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK: "llvm.br"(%[[C0]])[^bb[[HEADER:[0-9]+]]] : (index) -> ()
// CHECK: ^bb[[HEADER]](%[[IV:[0-9]+]]: index):
// CHECK: %[[C4:[0-9]+]] = "llvm.mlir.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[SHIFTED_UB:[0-9]+]] = "llvm.add"(%[[UB]], %[[C4]]) : (index, index) -> index
// CHECK: %[[PRED:[0-9]+]] = llvm.icmp "slt" %[[IV]], %[[SHIFTED_UB]] : index
// CHECK: "llvm.cond_br"(%[[PRED]])[^bb[[BODY:[0-9]+]], ^bb[[EXIT:[0-9]+]]] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK: ^bb[[BODY]]:
// CHECK: "memref.store"(%[[F0]], %[[OUT]], %[[IV]]) : (f32, memref<?xf32>, index) -> ()
// CHECK: %[[C1:[0-9]+]] = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// CHECK: %[[NEXT:[0-9]+]] = "llvm.add"(%[[IV]], %[[C1]]) : (index, index) -> index
// CHECK: "llvm.br"(%[[NEXT]])[^bb[[HEADER]]] : (index) -> ()
// CHECK: ^bb[[EXIT]]:
// CHECK: func.return
// CHECK-NOT: d_affine.for
