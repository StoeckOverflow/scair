#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<()[s0] -> (s0)>
#map2 = affine_map<(d0)[s0] -> (d0 + 3, s0)>
builtin.module {
  func.func @ordinary_tail(%0: index, %1: index, %2: memref<?xf32>) {
    %3 = "arith.muli"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %4 = "arith.constant"() <{value = 0 : index}> : () -> index
    %5 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
    affine.for %7 = #map(%6) to #map1()[%3] step 3 {
      affine.for %8 = #map(%7) to min #map2(%7)[%3] step 1 {
        "memref.store"(%5, %2, %8) : (f32, memref<?xf32>, index) -> ()
      }
      affine.yield
    }
    func.return
  }
}
