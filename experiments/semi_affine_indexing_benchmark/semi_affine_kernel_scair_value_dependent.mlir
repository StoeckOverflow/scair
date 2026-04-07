builtin.module {
  func.func @semi_affine_fill_and_sum(
    %stride0 : index,
    %stride1 : index,
    %flat_nat : !dtensor.nat,
    %out_nat : !dtensor.nat,
    %flat : !d_memref.memref<[%flat_nat], f32>,
    %out : !d_memref.memref<[%out_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat

    %buf = d_memref.reinterpret_cast %flat
      : !d_memref.memref<[%flat_nat], f32>
        to !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c256) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index {
        d_memref.store %f1, %buf[%i, %j] : f32, !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c256) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
        %next = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %next : (f32)
      }
      d_affine.yield %inner : (f32)
    }

    d_memref.store %sum, %out[%c0] : f32, !d_memref.memref<[%out_nat], f32>
    "func.return"() : () -> ()
  }
}
