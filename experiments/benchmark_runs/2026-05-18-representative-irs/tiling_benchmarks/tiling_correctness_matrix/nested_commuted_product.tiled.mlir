#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[] -> (d0 + 7)>
builtin.module {
  func.func @nested_commuted_product(%0: memref<?xf32>) {
    %1 = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
    %2 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %3 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
    %4 = "dtensor.nat.mul"(%2, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %5 = "dtensor.nat.mul"(%4, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %6 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
    %8 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %9 = "arith.constant"() <{value = 0 : index}> : () -> index
    %10 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    d_affine.for %11 = #map(%9) to #map(%6) step 7 : i32 {
      d_affine.for %12 = #map(%11) to #map1(%11) step 1 : i32 {
        "memref.store"(%8, %0, %12) : (f32, memref<?xf32>, index) -> ()
        d_affine.yield
      }
      d_affine.yield
    }
    func.return
  }
}
