// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-separable-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=SEPARABLE
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize,cse,dce,canonicalize-d-tensor-nat-products,dependent-context-band-separable-tile,d-affine-to-affine-compatible,validate-d-affine-dynamic-steps,canonicalize,cse,dce,lower-d-memref-to-llvm | filecheck %s --check-prefix=LOWERED

builtin.module {
  func.func @matmul_outer_separable(
    %m0_nat : !d_tensor.nat,
    %m1_nat : !d_tensor.posnat,
    %n0_nat : !d_tensor.nat,
    %n1_nat : !d_tensor.posnat,
    %k_nat : !d_tensor.nat,
    %Aflat : !d_memref.memref<[], f32>,
    %Bflat : !d_memref.memref<[], f32>,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %m_nat = "d_tensor.nat.mul"(%m0_nat, %m1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %n_nat = "d_tensor.nat.mul"(%n0_nat, %n1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %m = "d_tensor.shape.to_index"(%m_nat) : (!d_tensor.nat) -> index
    %n = "d_tensor.shape.to_index"(%n_nat) : (!d_tensor.nat) -> index
    %k = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %A = d_memref.reinterpret_cast %Aflat
      : !d_memref.memref<[], f32> to !d_memref.memref<[%m_nat, %k_nat], f32, offset: 0, strides: [%k, %c1]>
    %B = d_memref.reinterpret_cast %Bflat
      : !d_memref.memref<[], f32> to !d_memref.memref<[%k_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32> to !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%m_nat, %k_nat], f32, offset: 0, strides: [%k, %c1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%m_nat, %n_nat], f32, offset: 0, strides: [%n, %c1]>
        d_affine.yield
      }
      d_affine.yield
    }
    "func.return"() : () -> ()
  }
}

// SEPARABLE-LABEL: func.func @matmul_outer_separable
// SEPARABLE: d_affine.for %[[I_TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : index
// SEPARABLE: "d_affine.if"(%[[I_TILE]]
// SEPARABLE: d_affine.for %[[I:[0-9]+]] = #map(%[[I_TILE]]) to #map
// SEPARABLE: d_affine.for %[[J_TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : index
// SEPARABLE: "d_affine.if"(%[[J_TILE]]
// SEPARABLE: d_affine.for %[[J:[0-9]+]] = #map(%[[J_TILE]]) to #map
// SEPARABLE: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 : index iter_args
// SEPARABLE: arith.minsi
// SEPARABLE-NOT: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : index iter_args

// LOWERED-LABEL: func.func @matmul_outer_separable
// LOWERED-NOT: d_affine.if
// LOWERED-NOT: affine.if
// LOWERED-NOT: d_affine.for
// LOWERED-NOT: affine.for
// LOWERED: llvm.cond_br
