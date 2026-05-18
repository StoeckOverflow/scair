#map = affine_map<(d0)[] -> (d0)>
builtin.module {
  func.func @conv2d_full_factorized_tiling(%0: !dtensor.nat, %1: !dtensor.posnat, %2: !dtensor.nat, %3: !dtensor.posnat, %4: !dtensor.nat, %5: !dtensor.nat, %6: !dtensor.nat, %7: !dtensor.posnat, %8: !dtensor.posnat, %9: !dtensor.posnat, %10: !dtensor.nat, %11: !dtensor.posnat, %12: !dtensor.nat, %13: !dtensor.posnat, %14: !d_memref.memref<[], f32>, %15: !d_memref.memref<[], f32>, %16: !d_memref.memref<[], f32>) {
    %17 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %18 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %19 = "dtensor.nat.mul"(%6, %7) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %20 = "dtensor.nat.mul"(%10, %11) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %21 = "dtensor.nat.mul"(%12, %13) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %22 = "dtensor.nat.mul"(%8, %9) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
    %23 = "dtensor.nat.mul"(%3, %22) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
    %24 = "dtensor.nat.mul"(%2, %23) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %25 = "arith.constant"() <{value = 0 : index}> : () -> index
    %26 = "arith.constant"() <{value = 1 : index}> : () -> index
    %27 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %28 = "dtensor.shape.to_index"(%17) : (!dtensor.nat) -> index
    %29 = "dtensor.shape.to_index"(%18) : (!dtensor.nat) -> index
    %30 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
    %31 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
    %32 = "dtensor.shape.to_index"(%19) : (!dtensor.nat) -> index
    %33 = "dtensor.shape.to_index"(%9) : (!dtensor.posnat) -> index
    %34 = "dtensor.shape.to_index"(%20) : (!dtensor.nat) -> index
    %35 = "dtensor.shape.to_index"(%21) : (!dtensor.nat) -> index
    %36 = "dtensor.shape.to_index"(%22) : (!dtensor.posnat) -> index
    %37 = "dtensor.shape.to_index"(%24) : (!dtensor.nat) -> index
    %38 = "arith.muli"(%30, %31) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %39 = "arith.muli"(%29, %38) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %40 = "arith.muli"(%34, %35) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %41 = "arith.muli"(%32, %40) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %42 = d_memref.reinterpret_cast %14
    : !d_memref.memref<[], f32> to !d_memref.memref<[%17, %18, %20, %21, %8, %9], f32, offset: 0, strides: [%39, %38, %31, %26, %31, %26]>
    %43 = d_memref.reinterpret_cast %15
    : !d_memref.memref<[], f32> to !d_memref.memref<[%19, %18, %8, %9], f32, offset: 0, strides: [%37, %36, %33, %26]>
    %44 = d_memref.reinterpret_cast %16
    : !d_memref.memref<[], f32> to !d_memref.memref<[%17, %19, %20, %21], f32, offset: 0, strides: [%41, %40, %35, %26]>
    %45 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
    d_affine.for %46 = #map(%25) to #map(%28) step %45 : index {
      %47 = "arith.addi"(%46, %45) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
      d_affine.for %48 = #map(%46) to #map(%47) step 1 : index {
        %49 = "dtensor.shape.to_index"(%7) : (!dtensor.posnat) -> index
        d_affine.for %50 = #map(%25) to #map(%32) step %49 : index {
          %51 = "arith.addi"(%50, %49) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
          d_affine.for %52 = #map(%50) to #map(%51) step 1 : index {
            %53 = "dtensor.shape.to_index"(%11) : (!dtensor.posnat) -> index
            d_affine.for %54 = #map(%25) to #map(%34) step %53 : index {
              %55 = "arith.addi"(%54, %53) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
              d_affine.for %56 = #map(%54) to #map(%55) step 1 : index {
                %57 = "dtensor.shape.to_index"(%13) : (!dtensor.posnat) -> index
                d_affine.for %58 = #map(%25) to #map(%35) step %57 : index {
                  %59 = "arith.addi"(%58, %57) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
                  d_affine.for %60 = #map(%58) to #map(%59) step 1 : index {
                    %61 = d_affine.for %62 = #map(%25) to #map(%37) step %33 : index iter_args(%63 = %27 : f32) {
                      %64 = "arith.addi"(%62, %33) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
                      %65 = d_affine.for %66 = #map(%62) to #map(%64) step 1 : i32 iter_args(%67 = %63 : f32) {
                        %68 = "arith.divui"(%66, %36) : (index, index) -> index
                        %69 = "arith.remui"(%66, %36) : (index, index) -> index
                        %70 = "arith.divui"(%69, %33) : (index, index) -> index
                        %71 = "arith.remui"(%69, %33) : (index, index) -> index
                        %72 = d_memref.load %42[%48, %68, %56, %60, %70, %71] : !d_memref.memref<[%17, %18, %20, %21, %8, %9], f32, offset: 0, strides: [%39, %38, %31, %26, %31, %26]> -> f32
                        %73 = d_memref.load %43[%52, %68, %70, %71] : !d_memref.memref<[%19, %18, %8, %9], f32, offset: 0, strides: [%37, %36, %33, %26]> -> f32
                        %74 = "arith.mulf"(%72, %73) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                        %75 = "arith.addf"(%67, %74) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
                        d_affine.yield %75 : (f32)
                      }
                      d_affine.yield %65 : (f32)
                    }
                    d_memref.store %61, %44[%48, %52, %56, %60] : f32, !d_memref.memref<[%17, %19, %20, %21], f32, offset: 0, strides: [%41, %40, %35, %26]>
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
