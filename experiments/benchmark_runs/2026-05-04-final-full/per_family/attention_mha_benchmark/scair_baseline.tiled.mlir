#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func private @bench_expf(f32) -> f32
  func.func private @bench_inv_sqrt_index(index) -> f32
  func.func @attention_mha(%0: index, %1: index, %2: index, %3: index, %4: memref<?xf32>, %5: memref<?xf32>, %6: memref<?xf32>, %7: memref<?xf32>, %8: memref<?xf32>, %9: memref<?xf32>, %10: memref<?xf32>) attributes {scair.emit_descriptor_pointer_interface = true} {
    %11 = "arith.constant"() <{value = 0 : index}> : () -> index
    %12 = "arith.constant"() <{value = 1 : index}> : () -> index
    %13 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %14 = "arith.constant"() <{value = -3.40282347E38 : f32}> : () -> f32
    %15 = "arith.muli"(%2, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %16 = "arith.muli"(%1, %15) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %17 = "arith.muli"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %18 = "arith.muli"(%2, %17) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %19 = memref.reinterpret_cast %4 to
offset: [%11],
sizes: [%0, %1, %2, %3],
strides: [%16, %15, %3, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %20 = memref.reinterpret_cast %5 to
offset: [%11],
sizes: [%0, %1, %2, %3],
strides: [%16, %15, %3, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %21 = memref.reinterpret_cast %6 to
offset: [%11],
sizes: [%0, %1, %2, %3],
strides: [%16, %15, %3, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %22 = memref.reinterpret_cast %7 to
offset: [%11],
sizes: [%0, %2, %1, %1],
strides: [%18, %17, %1, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %23 = memref.reinterpret_cast %8 to
offset: [%11],
sizes: [%0, %2, %1, %1],
strides: [%18, %17, %1, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %24 = memref.reinterpret_cast %9 to
offset: [%11],
sizes: [%0, %1, %2, %3],
strides: [%16, %15, %3, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %25 = memref.reinterpret_cast %10 to
offset: [%11],
sizes: [%0, %1, %2, %3],
strides: [%16, %15, %3, %12]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>
    %26 = "func.call"(%3) <{callee = @bench_inv_sqrt_index}> : (index) -> f32
    affine.for %27 = #map(%11) to #map(%0) step 1 : index {
affine.for %28 = #map(%11) to #map(%2) step 1 : index {
affine.for %29 = #map(%11) to #map(%1) step 1 : index {
affine.for %30 = #map(%11) to #map(%1) step 1 : index {
%31 = affine.for %32 = #map(%11) to #map(%3) step 1 : index iter_args(%33 = %13 : f32) {
%34 = "memref.load"(%19, %27, %29, %28, %32) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%35 = "memref.load"(%20, %27, %30, %28, %32) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%36 = "arith.mulf"(%34, %35) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%37 = "arith.addf"(%33, %36) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
affine.yield %37 : f32
            }
%38 = "arith.mulf"(%31, %26) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
"memref.store"(%38, %22, %27, %28, %29, %30) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
          }
        }
      }
    }
    affine.for %39 = #map(%11) to #map(%0) step 1 : index {
affine.for %40 = #map(%11) to #map(%2) step 1 : index {
affine.for %41 = #map(%11) to #map(%1) step 1 : index {
%42 = affine.for %43 = #map(%11) to #map(%1) step 1 : index iter_args(%44 = %14 : f32) {
%45 = "memref.load"(%22, %39, %40, %41, %43) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%46 = "arith.maximumf"(%44, %45) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
affine.yield %46 : f32
          }
%47 = affine.for %48 = #map(%11) to #map(%1) step 1 : index iter_args(%49 = %13 : f32) {
%50 = "memref.load"(%22, %39, %40, %41, %48) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%51 = "arith.subf"(%50, %42) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%52 = "func.call"(%51) <{callee = @bench_expf}> : (f32) -> f32
"memref.store"(%52, %23, %39, %40, %41, %48) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
%53 = "arith.addf"(%49, %52) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
affine.yield %53 : f32
          }
affine.for %54 = #map(%11) to #map(%1) step 1 : index {
%55 = "memref.load"(%23, %39, %40, %41, %54) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%56 = "arith.divf"(%55, %47) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
"memref.store"(%56, %23, %39, %40, %41, %54) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
          }
        }
      }
    }
    affine.for %57 = #map(%11) to #map(%0) step 1 : index {
affine.for %58 = #map(%11) to #map(%1) step 1 : index {
affine.for %59 = #map(%11) to #map(%2) step 1 : index {
affine.for %60 = #map(%11) to #map(%3) step 1 : index {
%61 = affine.for %62 = #map(%11) to #map(%1) step 1 : index iter_args(%63 = %13 : f32) {
%64 = "memref.load"(%23, %57, %59, %58, %62) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%65 = "memref.load"(%21, %57, %62, %59, %60) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
%66 = "arith.mulf"(%64, %65) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%67 = "arith.addf"(%63, %66) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
affine.yield %67 : f32
            }
"memref.store"(%61, %24, %57, %58, %59, %60) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
          }
        }
affine.for %68 = #map(%11) to #map(%2) step 1 : index {
affine.for %69 = #map(%11) to #map(%3) step 1 : index {
%70 = "memref.load"(%24, %57, %58, %68, %69) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> f32
"memref.store"(%70, %25, %57, %58, %68, %69) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
          }
        }
      }
    }
    func.return
  }
}
