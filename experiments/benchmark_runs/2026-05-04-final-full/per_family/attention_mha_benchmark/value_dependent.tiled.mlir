#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func private @bench_expf(f32) -> f32
  func.func private @bench_inv_sqrt_index(index) -> f32
  func.func @attention_mha(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat, %3: !dtensor.nat, %4: !d_memref.memref<[], f32>, %5: !d_memref.memref<[], f32>, %6: !d_memref.memref<[], f32>, %7: !d_memref.memref<[], f32>, %8: !d_memref.memref<[], f32>, %9: !d_memref.memref<[], f32>, %10: !d_memref.memref<[], f32>) attributes {scair.emit_bare_interface = true} {
    %11 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %12 = "arith.constant"() <{value = 0 : index}> : () -> index
    %13 = "arith.constant"() <{value = 1 : index}> : () -> index
    %14 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %15 = "arith.constant"() <{value = -3.40282347E38 : f32}> : () -> f32
    %16 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
    %17 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %18 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
    %19 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %20 = "dtensor.shape.to_index"(%11) : (!dtensor.nat) -> index
    %21 = "arith.muli"(%17, %20) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %22 = "arith.muli"(%17, %17) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %23 = "arith.muli"(%18, %22) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %24 = d_memref.reinterpret_cast %4
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
    %25 = d_memref.reinterpret_cast %5
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
    %26 = d_memref.reinterpret_cast %6
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
    %27 = d_memref.reinterpret_cast %7
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]>
    %28 = d_memref.reinterpret_cast %8
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]>
    %29 = d_memref.reinterpret_cast %10
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
    %30 = d_memref.reinterpret_cast %9
    : !d_memref.memref<[], f32> to !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
    %31 = "func.call"(%19) <{callee = @bench_inv_sqrt_index}> : (index) -> f32
    d_affine.for %32 = #map(%12) to #map(%16) step 1 : index {
d_affine.for %33 = #map(%12) to #map(%18) step 1 : index {
%34 = "arith.muli"(%33, %19) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
d_affine.for %35 = #map(%12) to #map(%17) step 1 : index {
d_affine.for %36 = #map(%12) to #map(%17) step 1 : index {
%37 = d_affine.for %38 = #map(%12) to #map(%19) step 1 : index iter_args(%39 = %14 : f32) {
%40 = "arith.addi"(%34, %38) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%41 = d_memref.load %24[%32, %35, %40] : !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]> -> f32
%42 = d_memref.load %25[%32, %36, %40] : !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]> -> f32
%43 = "arith.mulf"(%41, %42) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%44 = "arith.addf"(%39, %43) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_affine.yield %44 : (f32)
            }
%45 = "arith.mulf"(%37, %31) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_memref.store %45, %27[%32, %33, %35, %36] : f32, !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]>
d_affine.yield
          }
d_affine.yield
        }
d_affine.yield
      }
d_affine.yield
    }
    d_affine.for %46 = #map(%12) to #map(%16) step 1 : index {
d_affine.for %47 = #map(%12) to #map(%18) step 1 : index {
d_affine.for %48 = #map(%12) to #map(%17) step 1 : index {
%49 = d_affine.for %50 = #map(%12) to #map(%17) step 1 : index iter_args(%51 = %15 : f32) {
%52 = d_memref.load %27[%46, %47, %48, %50] : !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]> -> f32
%53 = "arith.maximumf"(%51, %52) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_affine.yield %53 : (f32)
          }
%54 = d_affine.for %55 = #map(%12) to #map(%17) step 1 : index iter_args(%56 = %14 : f32) {
%57 = d_memref.load %27[%46, %47, %48, %55] : !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]> -> f32
%58 = "arith.subf"(%57, %49) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%59 = "func.call"(%58) <{callee = @bench_expf}> : (f32) -> f32
d_memref.store %59, %28[%46, %47, %48, %55] : f32, !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]>
%60 = "arith.addf"(%56, %59) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_affine.yield %60 : (f32)
          }
d_affine.for %61 = #map(%12) to #map(%17) step 1 : index {
%62 = d_memref.load %28[%46, %47, %48, %61] : !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]> -> f32
%63 = "arith.divf"(%62, %54) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_memref.store %63, %28[%46, %47, %48, %61] : f32, !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]>
d_affine.yield
          }
d_affine.yield
        }
d_affine.yield
      }
d_affine.yield
    }
    d_affine.for %64 = #map(%12) to #map(%16) step 1 : index {
d_affine.for %65 = #map(%12) to #map(%17) step 1 : index {
d_affine.for %66 = #map(%12) to #map(%18) step 1 : index {
%67 = "arith.muli"(%66, %19) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
d_affine.for %68 = #map(%12) to #map(%19) step 1 : index {
%69 = "arith.addi"(%67, %68) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%70 = d_affine.for %71 = #map(%12) to #map(%17) step 1 : index iter_args(%72 = %14 : f32) {
%73 = d_memref.load %28[%64, %66, %65, %71] : !d_memref.memref<[%0, %2, %1, %1], f32, offset: 0, strides: [%23, %22, %17, %13]> -> f32
%74 = d_memref.load %26[%64, %71, %69] : !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]> -> f32
%75 = "arith.mulf"(%73, %74) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
%76 = "arith.addf"(%72, %75) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
d_affine.yield %76 : (f32)
            }
d_memref.store %70, %30[%64, %65, %69] : f32, !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
d_affine.yield
          }
d_affine.yield
        }
%77 = d_affine.for %78 = #map(%12) to #map(%18) step 1 : i32 iter_args(%79 = %14 : f32) {
%80 = d_affine.for %81 = #map(%12) to #map(%19) step 1 : i32 iter_args(%82 = %79 : f32) {
%83 = "arith.muli"(%78, %19) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%84 = "arith.addi"(%83, %81) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
%85 = d_memref.load %30[%64, %65, %84] : !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]> -> f32
d_memref.store %85, %29[%64, %65, %84] : f32, !d_memref.memref<[%0, %1, %11], f32, offset: 0, strides: [%21, %20, %13]>
d_affine.yield %85 : (f32)
          }
d_affine.yield %80 : (f32)
        }
d_affine.yield
      }
d_affine.yield
    }
    func.return
  }
}
