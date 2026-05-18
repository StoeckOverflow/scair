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
      d_affine.for %45 = #map(%43) to #map(%44) step 1 : index {
        %46 = "dtensor.shape.to_index"(%6) : (!dtensor.posnat) -> index
        d_affine.for %47 = #map(%22) to #map(%29) step %46 : index {
          %48 = "arith.addi"(%47, %46) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          d_affine.for %49 = #map(%47) to #map(%48) step 1 : index {
            %50 = "dtensor.shape.to_index"(%10) : (!dtensor.posnat) -> index
            d_affine.for %51 = #map(%22) to #map(%31) step %50 : index {
              %52 = "arith.addi"(%51, %50) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
              d_affine.for %53 = #map(%51) to #map(%52) step 1 : index {
                %54 = "dtensor.shape.to_index"(%12) : (!dtensor.posnat) -> index
                d_affine.for %55 = #map(%22) to #map(%32) step %54 : index {
                  %56 = "arith.addi"(%55, %54) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
                  d_affine.for %57 = #map(%55) to #map(%56) step 1 : index {
                    %58 = d_affine.for %59 = #map(%22) to #map(%34) step 1 : index iter_args(%60 = %24 : f32) {
                      %61 = "arith.divui"(%59, %33) : (index, index) -> index
                      %62 = "arith.remui"(%59, %33) : (index, index) -> index
                      %63 = "arith.divui"(%62, %30) : (index, index) -> index
                      %64 = "arith.remui"(%62, %30) : (index, index) -> index
                      %65 = d_memref.load %39[%45, %61, %53, %57, %63, %64] : !d_memref.memref<[%16, %2, %18, %19, %7, %8], f32, offset: 0, strides: [%36, %35, %28, %23, %28, %23]> -> f32
                      %66 = d_memref.load %40[%49, %61, %63, %64] : !d_memref.memref<[%17, %2, %7, %8], f32, offset: 0, strides: [%34, %33, %30, %23]> -> f32
                      %67 = "arith.mulf"(%65, %66) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                      %68 = "arith.addf"(%60, %67) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                      d_affine.yield %68 : (f32)
                    }
                    d_memref.store %58, %41[%45, %49, %53, %57] : f32, !d_memref.memref<[%16, %17, %18, %19], f32, offset: 0, strides: [%38, %37, %32, %23]>
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
