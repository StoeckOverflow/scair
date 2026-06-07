// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p lower-baseline-control-flow-to-llvm | filecheck %s --check-prefix=BASE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p lower-baseline-control-flow-to-llvm | scair-opt --allow-unregistered-dialect --verify-diagnostics
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p lower-refined-control-flow-to-llvm | filecheck %s --check-prefix=REFINED
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p lower-refined-control-flow-to-llvm | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  func.func @baseline_if_dependent_result(
    %cond: i1,
    %n: !dtensor.nat,
    %a: !d_memref.memref<[%n], f32>,
    %b: !d_memref.memref<[%n], f32>
  ) -> !d_memref.memref<[%n], f32> {
    %r = "scf.if"(%cond) ({
      "scf.yield"(%a) : (!d_memref.memref<[%n], f32>) -> ()
    }, {
      "scf.yield"(%b) : (!d_memref.memref<[%n], f32>) -> ()
    }) : (i1) -> !d_memref.memref<[%n], f32>
    func.return %r : !d_memref.memref<[%n], f32>
  }

  func.func @refined_if_dependent_result(
    %cond: index,
    %n: !dtensor.nat,
    %a: !d_memref.memref<[%n], f32>,
    %b: !d_memref.memref<[%n], f32>
  ) -> !d_memref.memref<[%n], f32> {
    %r = "d_affine.if"(%cond) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
      d_affine.yield %a : (!d_memref.memref<[%n], f32>)
    }, {
      d_affine.yield %b : (!d_memref.memref<[%n], f32>)
    }) : (index) -> !d_memref.memref<[%n], f32>
    func.return %r : !d_memref.memref<[%n], f32>
  }
}

// BASE-LABEL: func.func @baseline_if_dependent_result(
// BASE-SAME: %[[COND:[0-9]+]]: i1, %[[N:[0-9]+]]: !dtensor.nat, %[[A:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>, %[[B:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>
// BASE-SAME: -> !d_memref.memref<[%[[N]]], f32>
// BASE: "llvm.cond_br"(%[[COND]])[^bb[[THEN:[0-9]+]], ^bb[[ELSE:[0-9]+]]]
// BASE: ^bb[[THEN]]:
// BASE: "llvm.br"(%[[A]])[^bb[[MERGE:[0-9]+]]] : (!d_memref.memref<[%[[N]]], f32>) -> ()
// BASE: ^bb[[ELSE]]:
// BASE: "llvm.br"(%[[B]])[^bb[[MERGE]]] : (!d_memref.memref<[%[[N]]], f32>) -> ()
// BASE: ^bb[[MERGE]](%[[R:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>):
// BASE: func.return %[[R]] : !d_memref.memref<[%[[N]]], f32>
// BASE-LABEL: func.func @refined_if_dependent_result(
// BASE: "d_affine.if"

// REFINED-LABEL: func.func @refined_if_dependent_result(
// REFINED-SAME: %[[COND:[0-9]+]]: index, %[[N:[0-9]+]]: !dtensor.nat, %[[A:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>, %[[B:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>
// REFINED-SAME: -> !d_memref.memref<[%[[N]]], f32>
// REFINED: %[[ZERO:[0-9]+]] = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// REFINED: %[[PRED:[0-9]+]] = llvm.icmp "sge" %[[COND]], %[[ZERO]] : index
// REFINED: "llvm.cond_br"(%[[PRED]])[^bb[[THEN:[0-9]+]], ^bb[[ELSE:[0-9]+]]]
// REFINED: ^bb[[THEN]]:
// REFINED: "llvm.br"(%[[A]])[^bb[[MERGE:[0-9]+]]] : (!d_memref.memref<[%[[N]]], f32>) -> ()
// REFINED: ^bb[[ELSE]]:
// REFINED: "llvm.br"(%[[B]])[^bb[[MERGE]]] : (!d_memref.memref<[%[[N]]], f32>) -> ()
// REFINED: ^bb[[MERGE]](%[[R:[0-9]+]]: !d_memref.memref<[%[[N]]], f32>):
// REFINED: func.return %[[R]] : !d_memref.memref<[%[[N]]], f32>
// REFINED-NOT: d_affine.if
