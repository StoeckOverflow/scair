#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @broadcast_affine_2d(%0: index, %1: index, %2: memref<?xi64>, %3: memref<?xi64>, %4: memref<?xi64>, %5: memref<?xi64>) attributes {scair.emit_descriptor_pointer_interface = true} {
    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
    %7 = "arith.constant"() <{value = 1 : index}> : () -> index
    %8 = memref.reinterpret_cast %2 to
offset: [%6],
sizes: [%0, %1],
strides: [%1, %7]
    : memref<?xi64> to memref<?x?xi64, strided<[?, ?], offset: 0>>
    %9 = memref.reinterpret_cast %3 to
offset: [%6],
sizes: [%1],
strides: [%7]
    : memref<?xi64> to memref<?xi64, strided<[?], offset: 0>>
    %10 = memref.reinterpret_cast %4 to
offset: [%6],
sizes: [%1],
strides: [%7]
    : memref<?xi64> to memref<?xi64, strided<[?], offset: 0>>
    %11 = memref.reinterpret_cast %5 to
offset: [%6],
sizes: [%0, %1],
strides: [%1, %7]
    : memref<?xi64> to memref<?x?xi64, strided<[?, ?], offset: 0>>
    affine.for %12 = #map(%6) to #map(%0) step 1 : index {
affine.for %13 = #map(%6) to #map(%1) step 1 : index {
%14 = "memref.load"(%8, %12, %13) : (memref<?x?xi64, strided<[?, ?], offset: 0>>, index, index) -> i64
%15 = "memref.load"(%9, %13) : (memref<?xi64, strided<[?], offset: 0>>, index) -> i64
%16 = "memref.load"(%10, %13) : (memref<?xi64, strided<[?], offset: 0>>, index) -> i64
%17 = "arith.muli"(%14, %15) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
%18 = "arith.addi"(%17, %16) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
"memref.store"(%18, %11, %12, %13) : (i64, memref<?x?xi64, strided<[?, ?], offset: 0>>, index, index) -> ()
      }
    }
    func.return
  }
}
