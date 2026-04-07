builtin.module {
  func.func @matmul_dynamic(
    %n_nat : !dtensor.nat,
    %m_nat : !dtensor.nat,
    %k_nat : !dtensor.nat,
    %A : !d_memref.memref<[%n_nat, %k_nat], f32>,
    %B : !d_memref.memref<[%k_nat, %m_nat], f32>,
    %C : !d_memref.memref<[%n_nat, %m_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%n_nat, %k_nat], f32> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_nat, %m_nat], f32> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%n_nat, %m_nat], f32>
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
