#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 8, s0)>
builtin.module {
  func.func @conv2d_output_dim_tiling(%0: index, %1: index, %2: index, %3: index, %4: index, %5: index, %6: index, %7: index, %8: index, %9: index, %10: index, %11: index, %12: index, %13: memref<?xf32>, %14: memref<?xf32>, %15: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %16 = "arith.constant"() <{value = 0 : index}> : () -> index
    %17 = "arith.constant"() <{value = 1 : index}> : () -> index
    %18 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %19 = "arith.muli"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %20 = "arith.muli"(%5, %6) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %21 = "arith.muli"(%9, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %22 = "arith.muli"(%11, %12) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %23 = "arith.muli"(%3, %4) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %24 = "arith.muli"(%2, %23) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %25 = "arith.muli"(%7, %8) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %26 = "arith.muli"(%2, %25) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %27 = "arith.muli"(%21, %22) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %28 = "arith.muli"(%20, %27) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %29 = memref.reinterpret_cast %13 to
      offset: [%16],
      sizes: [%19, %2, %21, %22, %7, %8],
      strides: [%24, %23, %4, %17, %4, %17]
    : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %30 = memref.reinterpret_cast %14 to
      offset: [%16],
      sizes: [%20, %2, %7, %8],
      strides: [%26, %25, %8, %17]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %31 = memref.reinterpret_cast %15 to
      offset: [%16],
      sizes: [%19, %20, %21, %22],
      strides: [%28, %27, %22, %17]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    affine.for %32 = #map(%16) to #map(%19) step 8 {
      affine.for %33 = #map(%32) to min #map1(%32)[%19] step 1 {
        affine.for %34 = #map(%16) to #map(%20) step 8 {
          affine.for %35 = #map(%34) to min #map1(%34)[%20] step 1 {
            affine.for %36 = #map(%16) to #map(%21) step 8 {
              affine.for %37 = #map(%36) to min #map1(%36)[%21] step 1 {
                affine.for %38 = #map(%16) to #map(%22) step 8 {
                  affine.for %39 = #map(%38) to min #map1(%38)[%22] step 1 {
                    %40 = affine.for %41 = #map(%16) to #map(%26) step 1 iter_args(%42 = %18) -> (f32) {
                      %43 = "arith.divui"(%41, %25) : (index, index) -> index
                      %44 = "arith.remui"(%41, %25) : (index, index) -> index
                      %45 = "arith.divui"(%44, %8) : (index, index) -> index
                      %46 = "arith.remui"(%44, %8) : (index, index) -> index
                      %47 = "memref.load"(%29, %33, %43, %37, %39, %45, %46) : (memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>, index, index, index, index, index, index) -> f32
                      %48 = "memref.load"(%30, %35, %43, %45, %46) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
                      %49 = "arith.mulf"(%47, %48) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                      %50 = "arith.addf"(%42, %49) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                      affine.yield %50 : f32
                    }
                    "memref.store"(%40, %31, %33, %35, %37, %39) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
                    affine.yield
                  }
                  affine.yield
                }
                affine.yield
              }
              affine.yield
            }
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
