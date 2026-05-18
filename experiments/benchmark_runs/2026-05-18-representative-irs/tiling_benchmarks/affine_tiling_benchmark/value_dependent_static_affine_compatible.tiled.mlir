#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[] -> (d0 + 3)>
builtin.module {
  func.func @affine_value_dependent_static_product(%0: !dtensor.nat, %1: memref<?xf32>) {
    %2 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %3 = "dtensor.nat.mul"(%2, %0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %4 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %5 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
    affine.for %7 = #map(%6) to #map(%4) step 3 {
      affine.for %8 = #map(%7) to #map1(%7) step 1 {
        "memref.store"(%5, %1, %8) : (f32, memref<?xf32>, index) -> ()
        affine.yield
      }
      affine.yield
    }
    func.return
  }
}
