#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @conv2d_output_dim_tiling(%0: !dtensor.nat, %1: !dtensor.posnat, %2: !dtensor.nat, %3: !dtensor.nat, %4: !dtensor.nat, %5: !dtensor.nat, %6: !dtensor.posnat, %7: !dtensor.posnat, %8: !dtensor.posnat, %9: !dtensor.nat, %10: !dtensor.posnat, %11: !dtensor.nat, %12: !dtensor.posnat, %13: !d_memref.memref<[], f32>, %14: !d_memref.memref<[], f32>, %15: !d_memref.memref<[], f32>) {
    %16 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %17 = "dtensor.nat.mul"(%5, %6) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %18 = "dtensor.nat.mul"(%9, %10) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %19 = "dtensor.nat.mul"(%11, %12) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %20 = "dtensor.nat.mul"(%7, %8) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
    %21 = "dtensor.nat.mul"(%2, %20) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %22 = "arith.constant"() <{value = 0 : index}> : () -> index
    %23 = "arith.constant"() <{value = 1 : index}> : () -> index
    %24 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %25 = "dtensor.shape.to_index"(%16) : (!dtensor.nat) -> index
    %26 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
    %27 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
    %28 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
    %29 = "dtensor.shape.to_index"(%17) : (!dtensor.nat) -> index
    %30 = "dtensor.shape.to_index"(%8) : (!dtensor.posnat) -> index
    %31 = "dtensor.shape.to_index"(%18) : (!dtensor.nat) -> index
    %32 = "dtensor.shape.to_index"(%19) : (!dtensor.nat) -> index
    %33 = "dtensor.shape.to_index"(%20) : (!dtensor.posnat) -> index
    %34 = "dtensor.shape.to_index"(%21) : (!dtensor.nat) -> index
    %35 = "arith.muli"(%27, %28) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %36 = "arith.muli"(%26, %35) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %37 = "arith.muli"(%31, %32) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %38 = "arith.muli"(%29, %37) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %39 = d_memref.reinterpret_cast %13
    : !d_memref.memref<[], f32> to !d_memref.memref<[%16, %2, %18, %19, %7, %8], f32, offset: 0, strides: [%36, %35, %28, %23, %28, %23]>
    %40 = d_memref.reinterpret_cast %14
    : !d_memref.memref<[], f32> to !d_memref.memref<[%17, %2, %7, %8], f32, offset: 0, strides: [%34, %33, %30, %23]>
    %41 = d_memref.reinterpret_cast %15
    : !d_memref.memref<[], f32> to !d_memref.memref<[%16, %17, %18, %19], f32, offset: 0, strides: [%38, %37, %32, %23]>
    %42 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
    d_affine.for %43 = #map(%22) to #map(%25) step %42 : index {
      %44 = "arith.addi"(%43, %42) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
      %45 = "arith.minsi"(%44, %25) : (index, index) -> index
      d_affine.for %46 = #map(%43) to #map(%45) step 1 : index {
        %47 = "dtensor.shape.to_index"(%6) : (!dtensor.posnat) -> index
        d_affine.for %48 = #map(%22) to #map(%29) step %47 : index {
          %49 = "arith.addi"(%48, %47) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          %50 = "arith.minsi"(%49, %29) : (index, index) -> index
          d_affine.for %51 = #map(%48) to #map(%50) step 1 : index {
            %52 = "dtensor.shape.to_index"(%10) : (!dtensor.posnat) -> index
            d_affine.for %53 = #map(%22) to #map(%31) step %52 : index {
              %54 = "arith.addi"(%53, %52) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
              %55 = "arith.minsi"(%54, %31) : (index, index) -> index
              d_affine.for %56 = #map(%53) to #map(%55) step 1 : index {
                %57 = "dtensor.shape.to_index"(%12) : (!dtensor.posnat) -> index
                d_affine.for %58 = #map(%22) to #map(%32) step %57 : index {
                  %59 = "arith.addi"(%58, %57) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
                  %60 = "arith.minsi"(%59, %32) : (index, index) -> index
                  d_affine.for %61 = #map(%58) to #map(%60) step 1 : index {
                    %62 = d_affine.for %63 = #map(%22) to #map(%34) step 1 : index iter_args(%64 = %24 : f32) {
                      %65 = "arith.divui"(%63, %33) : (index, index) -> index
                      %66 = "arith.remui"(%63, %33) : (index, index) -> index
                      %67 = "arith.divui"(%66, %30) : (index, index) -> index
                      %68 = "arith.remui"(%66, %30) : (index, index) -> index
                      %69 = d_memref.load %39[%46, %65, %56, %61, %67, %68] : !d_memref.memref<[%16, %2, %18, %19, %7, %8], f32, offset: 0, strides: [%36, %35, %28, %23, %28, %23]> -> f32
                      %70 = d_memref.load %40[%51, %65, %67, %68] : !d_memref.memref<[%17, %2, %7, %8], f32, offset: 0, strides: [%34, %33, %30, %23]> -> f32
                      %71 = "arith.mulf"(%69, %70) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                      %72 = "arith.addf"(%64, %71) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                      d_affine.yield %72 : (f32)
                    }
                    d_memref.store %62, %41[%46, %51, %56, %61] : f32, !d_memref.memref<[%16, %17, %18, %19], f32, offset: 0, strides: [%38, %37, %32, %23]>
                    d_affine.yield
                  }
                  d_affine.yield
                }
                d_affine.yield
              }
              d_affine.yield
            }
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }
    func.return
  }
}
