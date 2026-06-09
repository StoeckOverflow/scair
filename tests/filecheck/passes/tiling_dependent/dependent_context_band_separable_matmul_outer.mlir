// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize,cse,dce,canonicalize-d-tensor-size-products,dependent-context-band-separable-tile,validate-d-affine-dynamic-steps,canonicalize,cse,dce | filecheck %s --check-prefix=SEPARABLE

builtin.module {
  func.func @matmul_outer_separable(
    %m0_size : !d_tensor.size,
    %m1_size : !d_tensor.pos_size,
    %n0_size : !d_tensor.size,
    %n1_size : !d_tensor.pos_size,
    %k_size : !d_tensor.size,
    %Aflat : !d_memref.memref<[], f32>,
    %Bflat : !d_memref.memref<[], f32>,
    %Cflat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %m_size = "d_tensor.size.mul"(%m0_size, %m1_size) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %n_size = "d_tensor.size.mul"(%n0_size, %n1_size) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %A = d_memref.reinterpret_cast %Aflat
      : !d_memref.memref<[], f32> to !d_memref.memref<[%m_size, %k_size], f32, offset: 0, strides: [%k_size, %c1]>
    %B = d_memref.reinterpret_cast %Bflat
      : !d_memref.memref<[], f32> to !d_memref.memref<[%k_size, %n_size], f32, offset: 0, strides: [%n_size, %c1]>
    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[], f32> to !d_memref.memref<[%m_size, %n_size], f32, offset: 0, strides: [%n_size, %c1]>
    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m_size) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%m_size, %k_size], f32, offset: 0, strides: [%k_size, %c1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_size, %n_size], f32, offset: 0, strides: [%n_size, %c1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%m_size, %n_size], f32, offset: 0, strides: [%n_size, %c1]>
        d_affine.yield
      }
      d_affine.yield
    }
    "func.return"() : () -> ()
  }
}

// SEPARABLE-LABEL: func.func @matmul_outer_separable
// SEPARABLE-SAME: %[[M1_NAT:[0-9]+]]: !d_tensor.pos_size
// SEPARABLE-SAME: %[[N0_NAT:[0-9]+]]: !d_tensor.size
// SEPARABLE-SAME: %[[N1_NAT:[0-9]+]]: !d_tensor.pos_size
// SEPARABLE: d_affine.for %[[I_TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[M1_NAT]] : !d_tensor.pos_size
// SEPARABLE: "d_affine.if"(%[[I_TILE]]
// SEPARABLE: d_affine.for %[[I:[0-9]+]] = #map(%[[I_TILE]]) to #map
// SEPARABLE: d_affine.for %[[J_TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[N1_NAT]] : !d_tensor.pos_size
// SEPARABLE: "d_affine.if"(%[[J_TILE]]
// SEPARABLE: d_affine.for %[[J:[0-9]+]] = #map(%[[J_TILE]]) to #map
// SEPARABLE: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 : index iter_args
// SEPARABLE: d_affine.min
// SEPARABLE-NOT: d_affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : index iter_args
