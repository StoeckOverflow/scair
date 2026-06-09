// RUN: ! scair-opt %s --allow-unregistered-dialect -p validate-d-affine-dynamic-steps 2>&1 | filecheck %s

// A nonnegative assertion on an unknown index value is not a strict positivity
// proof for dynamic d_affine steps.
builtin.module {
  func.func @non_strict_nonnegative_is_not_positive(%step: index) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 16 : index}> : () -> index
    %ok = "arith.cmpi"(%step, %c0) <{predicate = 5 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "step may be zero"}> : (i1) -> ()

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: d_affine.for dynamic step must be proven strictly positive before lowering
