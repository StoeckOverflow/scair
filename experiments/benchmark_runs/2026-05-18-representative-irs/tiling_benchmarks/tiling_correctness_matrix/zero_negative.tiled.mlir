#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @zero_negative(%0: memref<?xf32>) {
    %1 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %2 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %3 = "dtensor.nat.mul"(%2, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %4 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %5 = "arith.constant"() <{value = 0 : index}> : () -> index
    %6 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %7 = #map(%5) to #map(%4) step 1 : index {
      "memref.store"(%6, %0, %7) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    func.return
  }
}
