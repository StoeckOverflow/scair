#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 8, s0)>
#map2 = affine_map<()[s0] -> (s0)>
#map3 = affine_map<(d0)[s0] -> (d0 + 36, s0)>
builtin.module {
  func.func @conv2d_full_factorized_tiling(%0: index, %1: index, %2: index, %3: index, %4: index, %5: index, %6: index, %7: index, %8: index, %9: index, %10: index, %11: index, %12: index, %13: index, %14: memref<?xf32>, %15: memref<?xf32>, %16: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %17 = "arith.constant"() <{value = 0 : index}> : () -> index
    %18 = "arith.constant"() <{value = 1 : index}> : () -> index
    %19 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %20 = "arith.muli"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %21 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %22 = "arith.muli"(%6, %7) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %23 = "arith.muli"(%10, %11) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %24 = "arith.muli"(%12, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %25 = "arith.muli"(%4, %5) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %26 = "arith.muli"(%21, %25) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %27 = "arith.muli"(%8, %9) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %28 = "arith.muli"(%3, %27) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %29 = "arith.muli"(%2, %28) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %30 = "arith.muli"(%23, %24) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %31 = "arith.muli"(%22, %30) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %32 = memref.reinterpret_cast %14 to
      offset: [%17],
      sizes: [%20, %21, %23, %24, %8, %9],
      strides: [%26, %25, %5, %18, %5, %18]
    : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %33 = memref.reinterpret_cast %15 to
      offset: [%17],
      sizes: [%22, %21, %8, %9],
      strides: [%29, %27, %9, %18]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %34 = memref.reinterpret_cast %16 to
      offset: [%17],
      sizes: [%20, %22, %23, %24],
      strides: [%31, %30, %24, %18]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    affine.for %35 = #map(%17) to #map(%20) step 8 {
      affine.for %36 = #map(%35) to min #map1(%35)[%20] step 1 {
        affine.for %37 = #map(%17) to #map(%22) step 8 {
          affine.for %38 = #map(%37) to min #map1(%37)[%22] step 1 {
            affine.for %39 = #map(%17) to #map(%23) step 8 {
              affine.for %40 = #map(%39) to min #map1(%39)[%23] step 1 {
                affine.for %41 = #map(%17) to #map(%24) step 8 {
                  affine.for %42 = #map(%41) to min #map1(%41)[%24] step 1 {
                    %43 = affine.for %44 = #map(%17) to #map2()[%29] step 36 iter_args(%45 = %19) -> (f32) {
                      %46 = affine.for %47 = #map(%44) to min #map3(%44)[%29] step 1 iter_args(%48 = %45) -> (f32) {
                        %49 = "arith.divui"(%47, %27) : (index, index) -> index
                        %50 = "arith.remui"(%47, %27) : (index, index) -> index
                        %51 = "arith.divui"(%50, %9) : (index, index) -> index
                        %52 = "arith.remui"(%50, %9) : (index, index) -> index
                        %53 = "memref.load"(%32, %36, %49, %40, %42, %51, %52) : (memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>, index, index, index, index, index, index) -> f32
                        %54 = "memref.load"(%33, %38, %49, %51, %52) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
                        %55 = "arith.mulf"(%53, %54) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                        %56 = "arith.addf"(%48, %55) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                        affine.yield %56 : f32
                      }
                      affine.yield %46 : f32
                    }
                    "memref.store"(%43, %34, %36, %38, %40, %42) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
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
