builtin.module {
  func.func @affine_value_dependent_product(
    %k0: index,
    %k1: index,
    %out: memref<?xf32>
  ) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
