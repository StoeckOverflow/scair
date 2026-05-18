#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 64, s0)>
#map2 = affine_map<()[s0] -> (s0)>
builtin.module {
  func.func @matmul_full_factorized_tiling(%0: index, %1: index, %2: index, %3: index, %4: index, %5: index, %6: memref<?xf32>, %7: memref<?xf32>, %8: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %9 = "arith.constant"() <{value = 0 : index}> : () -> index
    %10 = "arith.constant"() <{value = 1 : index}> : () -> index
    %11 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %12 = "arith.muli"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %13 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %14 = "arith.muli"(%4, %5) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %15 = memref.reinterpret_cast %6 to
      offset: [%9],
      sizes: [%12, %14],
      strides: [%14, %10]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %16 = memref.reinterpret_cast %7 to
      offset: [%9],
      sizes: [%14, %13],
      strides: [%13, %10]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %17 = memref.reinterpret_cast %8 to
      offset: [%9],
      sizes: [%12, %13],
      strides: [%13, %10]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    affine.for %18 = #map(%9) to #map(%12) step 64 {
      affine.for %19 = #map(%18) to min #map1(%18)[%12] step 1 {
        affine.for %20 = #map(%9) to #map(%13) step 64 {
          affine.for %21 = #map(%20) to min #map1(%20)[%13] step 1 {
            %22 = affine.for %23 = #map(%9) to #map2()[%14] step 64 iter_args(%24 = %11) -> (f32) {
              %25 = affine.for %26 = #map(%23) to min #map1(%23)[%14] step 1 iter_args(%27 = %24) -> (f32) {
                %28 = "memref.load"(%15, %19, %26) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
                %29 = "memref.load"(%16, %26, %21) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
                %30 = "arith.mulf"(%28, %29) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                %31 = "arith.addf"(%27, %30) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                affine.yield %31 : f32
              }
              affine.yield %25 : f32
            }
            "memref.store"(%22, %17, %19, %21) : (f32, memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> ()
          }
          affine.yield
        }
      }
      affine.yield
    }
    func.return
  }
}
