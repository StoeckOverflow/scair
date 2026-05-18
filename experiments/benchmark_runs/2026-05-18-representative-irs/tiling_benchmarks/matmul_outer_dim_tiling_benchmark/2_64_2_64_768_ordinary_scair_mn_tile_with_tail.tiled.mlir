#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 64, s0)>
builtin.module {
  func.func @matmul_outer_dim_tiling(%0: index, %1: index, %2: index, %3: index, %4: index, %5: memref<?xf32>, %6: memref<?xf32>, %7: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %8 = "arith.constant"() <{value = 0 : index}> : () -> index
    %9 = "arith.constant"() <{value = 1 : index}> : () -> index
    %10 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %11 = "arith.muli"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %12 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %13 = memref.reinterpret_cast %5 to
      offset: [%8],
      sizes: [%11, %4],
      strides: [%4, %9]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %14 = memref.reinterpret_cast %6 to
      offset: [%8],
      sizes: [%4, %12],
      strides: [%12, %9]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %15 = memref.reinterpret_cast %7 to
      offset: [%8],
      sizes: [%11, %12],
      strides: [%12, %9]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    affine.for %16 = #map(%8) to #map(%11) step 64 {
      affine.for %17 = #map(%16) to min #map1(%16)[%11] step 1 {
        affine.for %18 = #map(%8) to #map(%12) step 64 {
          affine.for %19 = #map(%18) to min #map1(%18)[%12] step 1 {
            %20 = affine.for %21 = #map(%8) to #map(%4) step 1 iter_args(%22 = %10) -> (f32) {
              %23 = "memref.load"(%13, %17, %21) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
              %24 = "memref.load"(%14, %21, %19) : (memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> f32
              %25 = "arith.mulf"(%23, %24) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %26 = "arith.addf"(%22, %25) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              affine.yield %26 : f32
            }
            "memref.store"(%20, %15, %17, %19) : (f32, memref<?x?xf32, strided<[?, ?], offset: ?>>, index, index) -> ()
          }
          affine.yield
        }
      }
      affine.yield
    }
    func.return
  }
}
