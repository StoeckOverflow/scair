#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @matmul_tiling(%0: index, %1: index, %2: index, %3: index, %4: memref<?xf32>, %5: memref<?xf32>, %6: memref<?xf32>) attributes {scair.emit_descriptor_pointer_interface = true} {
    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
    %8 = "arith.constant"() <{value = 1 : index}> : () -> index
    %9 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %10 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %11 = memref.reinterpret_cast %4 to
offset: [%7],
sizes: [%0, %10],
strides: [%10, %8]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>
    %12 = memref.reinterpret_cast %5 to
offset: [%7],
sizes: [%10, %1],
strides: [%1, %8]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>
    %13 = memref.reinterpret_cast %6 to
offset: [%7],
sizes: [%0, %1],
strides: [%1, %8]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>
    affine.for %14 = #map(%7) to #map(%0) step 1 : index {
affine.for %15 = #map(%7) to #map(%1) step 1 : index {
%16 = affine.for %17 = #map(%7) to #map(%10) step 1 : index iter_args(%18 = %9 : f32) {
%19 = "memref.load"(%11, %14, %17) : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
%20 = "memref.load"(%12, %17, %15) : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
%21 = "arith.mulf"(%19, %20) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%22 = "arith.addf"(%18, %21) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
affine.yield %22 : f32
        }
"memref.store"(%16, %13, %14, %15) : (f32, memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
      }
    }
    func.return
  }
}
