#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @affine_value_dependent_product(%0: !dtensor.nat, %1: !dtensor.posnat, %2: memref<?xf32>) {
    %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %4 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %5 = "arith.constant"() <{value = 0 : index}> : () -> index
    %6 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
    %8 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
    d_affine.for %9 = #map(%7) to #map(%4) step %8 : index {
      %10 = "arith.addi"(%9, %8) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
      d_affine.for %11 = #map(%9) to #map(%10) step 1 : i32 {
        "memref.store"(%6, %2, %11) : (f32, memref<?xf32>, index) -> ()
        d_affine.yield
      }
      d_affine.yield
    }
    func.return
  }
}
