// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-shape-products,dependent-exact-tile,dependent-tail-min-simplify,validate-d-affine-dynamic-steps,lower-refined-control-flow-to-llvm,lower-cf-assert-to-llvm,canonicalize,cse,dce | filecheck %s --implicit-check-not=d_tensor. --implicit-check-not=d_affine.for --implicit-check-not=cf.assert --implicit-check-not=arith.minsi --implicit-check-not=affine.min

builtin.module {
  func.func @runtime_checked_exact_tile(
      %k0_idx: index,
      %k1_idx: index) -> index {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %ub = "arith.muli"(%k0_idx, %k1_idx) : (index, index) -> index

    %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %c0 : index) {
      %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
      d_affine.yield %next : (index)
    }

    "func.return"(%sum) : (index) -> ()
  }
}

// CHECK-LABEL: func.func @runtime_checked_exact_tile
// CHECK: "llvm.cond_br"
// CHECK: llvm.call @abort()
// CHECK: "llvm.unreachable"
// CHECK: "arith.muli"
// CHECK-NOT: step 3
// CHECK: "llvm.br"
// CHECK: func.return
