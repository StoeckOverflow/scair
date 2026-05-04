#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @blocked_pack(%0: index, %1: index, %2: index, %3: index, %4: memref<?xi64>, %5: memref<?xi64>) attributes {scair.emit_descriptor_pointer_interface = true} {
    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
    %7 = "arith.constant"() <{value = 1 : index}> : () -> index
    %8 = "arith.muli"(%1, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %9 = "arith.muli"(%2, %8) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %10 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %11 = "arith.muli"(%1, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %12 = memref.reinterpret_cast %4 to
offset: [%6],
sizes: [%0, %1, %2, %3],
strides: [%9, %3, %8, %7]
    : memref<?xi64> to memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>
    %13 = memref.reinterpret_cast %5 to
offset: [%6],
sizes: [%0, %1, %2, %3],
strides: [%11, %10, %3, %7]
    : memref<?xi64> to memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>
    affine.for %14 = #map(%6) to #map(%0) step 1 : index {
affine.for %15 = #map(%6) to #map(%1) step 1 : index {
affine.for %16 = #map(%6) to #map(%2) step 1 : index {
affine.for %17 = #map(%6) to #map(%3) step 1 : index {
%18 = "memref.load"(%12, %14, %15, %16, %17) : (memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> i64
"memref.store"(%18, %13, %14, %15, %16, %17) : (i64, memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
          }
        }
      }
    }
    func.return
  }
}
