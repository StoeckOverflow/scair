// RUN: scair-opt %s -p lower-dmemref-to-llvm | filecheck %s

builtin.module {
  func.func @main() -> i32 {
    %n = "arith.constant"() <{value = 32 : index}> : () -> index
    %m = "arith.constant"() <{value = 32 : index}> : () -> index
    %k = "arith.constant"() <{value = 32 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %ret = "arith.constant"() <{value = 0 : i32}> : () -> i32

    %A_size = "arith.muli"(%n, %k) : (index, index) -> index
    %B_size = "arith.muli"(%k, %m) : (index, index) -> index
    %C_size = "arith.muli"(%n, %m) : (index, index) -> index
    %n_nat = "dtensor.index_to_nat"(%n) : (index) -> !dtensor.nat
    %m_nat = "dtensor.index_to_nat"(%m) : (index) -> !dtensor.nat
    %k_nat = "dtensor.index_to_nat"(%k) : (index) -> !dtensor.nat
    %A_size_nat = "dtensor.index_to_nat"(%A_size) : (index) -> !dtensor.nat
    %B_size_nat = "dtensor.index_to_nat"(%B_size) : (index) -> !dtensor.nat
    %C_size_nat = "dtensor.index_to_nat"(%C_size) : (index) -> !dtensor.nat

    %Aflat = d_memref.alloc : () -> !d_memref.memref<[%A_size_nat], f32>
    %Bflat = d_memref.alloc : () -> !d_memref.memref<[%B_size_nat], f32>
    %Cflat = d_memref.alloc : () -> !d_memref.memref<[%C_size_nat], f32>

    %A = d_memref.reinterpret_cast %Aflat
      : !d_memref.memref<[%A_size_nat], f32>
        to !d_memref.memref<[%n_nat, %k_nat], f32, offset: %c0, strides: [%k, 1]>
    %B = d_memref.reinterpret_cast %Bflat
      : !d_memref.memref<[%B_size_nat], f32>
        to !d_memref.memref<[%k_nat, %m_nat], f32, offset: %c0, strides: [%m, 1]>
    %C = d_memref.reinterpret_cast %Cflat
      : !d_memref.memref<[%C_size_nat], f32>
        to !d_memref.memref<[%n_nat, %m_nat], f32, offset: %c0, strides: [%m, 1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
        d_memref.store %f1, %A[%i, %j] : f32, !d_memref.memref<[%n_nat, %k_nat], f32, offset: %c0, strides: [%k, 1]>
        d_affine.yield
      }
      d_affine.yield
    }

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
        d_memref.store %f1, %B[%i, %j] : f32, !d_memref.memref<[%k_nat, %m_nat], f32, offset: %c0, strides: [%m, 1]>
        d_affine.yield
      }
      d_affine.yield
    }

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
        %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %f0 : f32) {
          %a = d_memref.load %A[%i, %p] : !d_memref.memref<[%n_nat, %k_nat], f32, offset: %c0, strides: [%k, 1]> -> f32
          %b = d_memref.load %B[%p, %j] : !d_memref.memref<[%k_nat, %m_nat], f32, offset: %c0, strides: [%m, 1]> -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          d_affine.yield %next : (f32)
        }
        d_memref.store %sum, %C[%i, %j] : f32, !d_memref.memref<[%n_nat, %m_nat], f32, offset: %c0, strides: [%m, 1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %checksum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %C[%i, %j] : !d_memref.memref<[%n_nat, %m_nat], f32, offset: %c0, strides: [%m, 1]> -> f32
        %next = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %next : (f32)
      }
      d_affine.yield %inner : (f32)
    }

    d_memref.dealloc %Aflat : !d_memref.memref<[%A_size_nat], f32>
    d_memref.dealloc %Bflat : !d_memref.memref<[%B_size_nat], f32>
    d_memref.dealloc %Cflat : !d_memref.memref<[%C_size_nat], f32>
    func.return %ret : i32
  }
}

// CHECK-LABEL: func.func @main() -> i32 {
// CHECK: llvm.call @malloc
// CHECK: llvm.getelementptr
// CHECK: llvm.fadd
// CHECK: llvm.return
