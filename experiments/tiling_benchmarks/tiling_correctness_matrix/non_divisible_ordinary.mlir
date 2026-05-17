builtin.module {
  func.func @non_divisible_ordinary(%out: memref<?xf32>) {
    %k0 = "arith.constant"() <{value = 5 : index}> : () -> index
    %k1 = "arith.constant"() <{value = 2 : index}> : () -> index
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
    }

    "func.return"() : () -> ()
  }
}
