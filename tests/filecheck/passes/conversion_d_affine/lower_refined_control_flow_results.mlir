// RUN: scair-opt %s --allow-unregistered-dialect -p lower-refined-control-flow-to-llvm | filecheck %s

builtin.module {
  func.func @result_if_and_multi_result_loop(%cond: index, %lb: index, %ub: index, %a: index, %b: index) -> (index, index, index) {
    %ifv = "d_affine.if"(%cond) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
      d_affine.yield %a : (index)
    }, {
      d_affine.yield %b : (index)
    }) : (index) -> index

    %r0, %r1 = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%x = %ifv : index, %y = %b : index) {
      %next_x = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%x] : (index)[index] -> index
      %next_y = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%next_x)[%y] : (index)[index] -> index
      d_affine.yield %next_x, %next_y : (index, index)
    }

    func.return %ifv, %r0, %r1 : index, index, index
  }
}

// CHECK-LABEL: func.func @result_if_and_multi_result_loop
// CHECK-SAME: %[[COND:[0-9]+]]: index, %[[LB:[0-9]+]]: index, %[[UB:[0-9]+]]: index, %[[A:[0-9]+]]: index, %[[B:[0-9]+]]: index
// CHECK: %[[ZERO:[0-9]+]] = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %[[PRED:[0-9]+]] = llvm.icmp "sge" %[[COND]], %[[ZERO]] : index
// CHECK: "llvm.cond_br"(%[[PRED]])[^bb[[THEN:[0-9]+]], ^bb[[ELSE:[0-9]+]]] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK: ^bb[[THEN]]:
// CHECK: "llvm.br"(%[[A]])[^bb[[MERGE:[0-9]+]]] : (index) -> ()
// CHECK: ^bb[[ELSE]]:
// CHECK: "llvm.br"(%[[B]])[^bb[[MERGE]]] : (index) -> ()
// CHECK: ^bb[[MERGE]](%[[IFV:[0-9]+]]: index):
// CHECK: "llvm.br"(%[[LB]], %[[IFV]], %[[B]])[^bb[[HEADER:[0-9]+]]] : (index, index, index) -> ()
// CHECK: ^bb[[HEADER]](%[[IV:[0-9]+]]: index, %[[X:[0-9]+]]: index, %[[Y:[0-9]+]]: index):
// CHECK: %[[CMP:[0-9]+]] = llvm.icmp "slt" %[[IV]], %[[UB]] : index
// CHECK: "llvm.cond_br"(%[[CMP]], %[[X]], %[[Y]])[^bb[[BODY:[0-9]+]], ^bb[[EXIT:[0-9]+]]] <{operandSegmentSizes = array<i32: 1, 0, 2>}> : (i1, index, index) -> ()
// CHECK: ^bb[[BODY]]:
// CHECK: %[[NX:[0-9]+]] = "llvm.add"(%[[IV]], %[[X]]) : (index, index) -> i64
// CHECK: %[[NY:[0-9]+]] = "llvm.add"(%[[NX]], %[[Y]]) : (i64, index) -> i64
// CHECK: %[[ONE:[0-9]+]] = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// CHECK: %[[NEXT_IV:[0-9]+]] = "llvm.add"(%[[IV]], %[[ONE]]) : (index, i64) -> i64
// CHECK: "llvm.br"(%[[NEXT_IV]], %[[NX]], %[[NY]])[^bb[[HEADER]]] : (i64, i64, i64) -> ()
// CHECK: ^bb[[EXIT]](%[[R0:[0-9]+]]: index, %[[R1:[0-9]+]]: index):
// CHECK: func.return %[[IFV]], %[[R0]], %[[R1]] : index, index, index
// CHECK-NOT: d_affine.if
// CHECK-NOT: d_affine.for
