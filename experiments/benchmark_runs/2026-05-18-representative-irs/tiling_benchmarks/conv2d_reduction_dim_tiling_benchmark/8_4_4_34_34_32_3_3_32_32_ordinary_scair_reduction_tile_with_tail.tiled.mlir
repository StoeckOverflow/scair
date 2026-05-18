#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<()[s0] -> (s0)>
#map2 = affine_map<(d0)[s0] -> (d0 + 36, s0)>
builtin.module {
  func.func @conv2d_reduction_dim_tiling(%0: index, %1: index, %2: index, %3: index, %4: index, %5: index, %6: index, %7: index, %8: index, %9: index, %10: memref<?xf32>, %11: memref<?xf32>, %12: memref<?xf32>) {
    %13 = "arith.constant"() <{value = 0 : index}> : () -> index
    %14 = "arith.constant"() <{value = 1 : index}> : () -> index
    %15 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %16 = "arith.muli"(%1, %2) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %17 = "arith.muli"(%3, %4) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %18 = "arith.muli"(%16, %17) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %19 = "arith.muli"(%6, %7) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %20 = "arith.muli"(%2, %19) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %21 = "arith.muli"(%1, %20) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %22 = "arith.muli"(%8, %9) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %23 = "arith.muli"(%5, %22) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %24 = memref.reinterpret_cast %10 to
offset: [%13],
sizes: [%0, %16, %8, %9, %6, %7],
strides: [%18, %17, %4, %14, %4, %14]
    : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %25 = memref.reinterpret_cast %11 to
offset: [%13],
sizes: [%5, %16, %6, %7],
strides: [%21, %19, %7, %14]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %26 = memref.reinterpret_cast %12 to
offset: [%13],
sizes: [%0, %5, %8, %9],
strides: [%23, %22, %9, %14]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    affine.for %27 = #map(%13) to #map(%0) step 1 {
      affine.for %28 = #map(%13) to #map(%5) step 1 {
        affine.for %29 = #map(%13) to #map(%8) step 1 {
          affine.for %30 = #map(%13) to #map(%9) step 1 {
            %31 = affine.for %32 = #map(%13) to #map1()[%21] step 36 iter_args(%33 = %15) -> (f32) {
              %34 = affine.for %35 = #map(%32) to min #map2(%32)[%21] step 1 iter_args(%36 = %33) -> (f32) {
                %37 = "arith.divui"(%35, %19) : (index, index) -> index
                %38 = "arith.remui"(%35, %19) : (index, index) -> index
                %39 = "arith.divui"(%38, %7) : (index, index) -> index
                %40 = "arith.remui"(%38, %7) : (index, index) -> index
                %41 = "memref.load"(%24, %27, %37, %29, %30, %39, %40) : (memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>, index, index, index, index, index, index) -> f32
                %42 = "memref.load"(%25, %28, %37, %39, %40) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
                %43 = "arith.mulf"(%41, %42) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                %44 = "arith.addf"(%36, %43) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                affine.yield %44 : f32
              }
              affine.yield %34 : f32
            }
            "memref.store"(%31, %26, %27, %28, %29, %30) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
            affine.yield
          }
          affine.yield
        }
        affine.yield
      }
      affine.yield
    }
    func.return
  }
}
