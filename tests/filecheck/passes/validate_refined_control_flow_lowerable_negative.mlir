// RUN: ! scair-opt %s --allow-unregistered-dialect -p validate-refined-control-flow-lowerable 2>&1 | filecheck %s

builtin.module {
  func.func @unsupported_mod_bound(%ub: index) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0 mod 4)>(%ub) step 1 : index {
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: lower-refined-control-flow-to-llvm cannot lower current IR
// CHECK: d_affine.for upper bound has unsupported affine map
// CHECK: mod affine expressions are not supported
