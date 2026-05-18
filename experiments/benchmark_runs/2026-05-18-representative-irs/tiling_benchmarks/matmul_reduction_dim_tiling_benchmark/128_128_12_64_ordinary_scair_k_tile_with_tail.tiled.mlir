#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<()[s0] -> (s0)>
#map2 = affine_map<(d0)[s0] -> (d0 + 64, s0)>
builtin.module {
  func.func @matmul_reduction_dim_tiling(%0: index, %1: index, %2: index, %3: index, %4: memref<?xf32>, %5: memref<?xf32>, %6: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
    %8 = "arith.constant"() <{value = 1 : index}> : () -> index
    %9 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %10 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %11 = memref.reinterpret_cast %4 to
      offset: [%7],
      sizes: [%0, %10],
      strides: [%10, %8]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %12 = memref.reinterpret_cast %5 to
      offset: [%7],
      sizes: [%10, %1],
      strides: [%1, %8]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %13 = memref.reinterpret_cast %6 to
      offset: [%7],
      sizes: [%0, %1],
      strides: [%1, %8]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    affine.for %14 = #map(%7) to #map(%0) step 1 {
      affine.for %15 = #map(%7) to #map(%1) step 1 {
        %16 = affine.for %17 = #map(%7) to #map1()[%10] step 64 iter_args(%18 = %9) -> (f32) {
          %19 = affine.for %20 = #map(%17) to min #map2(%17)[%10] step 1 iter_args(%21 = %18) -> (f32) {
            %22 = "memref.load"(%11, %14, %20) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
            %23 = "memref.load"(%12, %20, %15) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
            %24 = "arith.mulf"(%22, %23) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            %25 = "arith.addf"(%21, %24) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            affine.yield %25 : f32
          }
          affine.yield %19 : f32
        }
        "memref.store"(%16, %13, %14, %15) : (f32, memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> ()
      }
    }
    func.return
  }
}
