// RUN: scair-opt %s -p lower-d-memref-to-llvm | filecheck %s --implicit-check-not=llvm.extractvalue --implicit-check-not=llvm.insertvalue

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
    %n_nat = "d_tensor.index_to_nat"(%n) : (index) -> !d_tensor.nat
    %m_nat = "d_tensor.index_to_nat"(%m) : (index) -> !d_tensor.nat
    %k_nat = "d_tensor.index_to_nat"(%k) : (index) -> !d_tensor.nat
    %A_size_nat = "d_tensor.index_to_nat"(%A_size) : (index) -> !d_tensor.nat
    %B_size_nat = "d_tensor.index_to_nat"(%B_size) : (index) -> !d_tensor.nat
    %C_size_nat = "d_tensor.index_to_nat"(%C_size) : (index) -> !d_tensor.nat

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
// CHECK: %[[ONE:[0-9]+]] = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// CHECK: %[[N:[0-9]+]] = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// CHECK: %[[M:[0-9]+]] = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// CHECK: %[[K:[0-9]+]] = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// CHECK: %[[ZERO:[0-9]+]] = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %[[F0:[0-9]+]] = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK: %[[F1:[0-9]+]] = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// CHECK: %[[RET:[0-9]+]] = "llvm.mlir.constant"() <{value = 0 : i32}> : () -> i32
// CHECK: %[[A_SIZE:[0-9]+]] = "llvm.mul"(%[[N]], %[[K]]) : (i64, i64) -> i64
// CHECK: %[[B_SIZE:[0-9]+]] = "llvm.mul"(%[[K]], %[[M]]) : (i64, i64) -> i64
// CHECK: %[[C_SIZE:[0-9]+]] = "llvm.mul"(%[[N]], %[[M]]) : (i64, i64) -> i64
// CHECK: %[[A_BYTES_PTR:[0-9]+]] = "llvm.getelementptr"(%{{[0-9]+}}, %[[A_SIZE]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[A_BYTES:[0-9]+]] = "llvm.ptrtoint"(%[[A_BYTES_PTR]]) : (!llvm.ptr) -> i64
// CHECK: %[[A_PTR:[0-9]+]] = llvm.call @malloc(%[[A_BYTES]]) : (i64) -> !llvm.ptr
// CHECK: %[[B_BYTES_PTR:[0-9]+]] = "llvm.getelementptr"(%{{[0-9]+}}, %[[B_SIZE]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[B_BYTES:[0-9]+]] = "llvm.ptrtoint"(%[[B_BYTES_PTR]]) : (!llvm.ptr) -> i64
// CHECK: %[[B_PTR:[0-9]+]] = llvm.call @malloc(%[[B_BYTES]]) : (i64) -> !llvm.ptr
// CHECK: %[[C_BYTES_PTR:[0-9]+]] = "llvm.getelementptr"(%{{[0-9]+}}, %[[C_SIZE]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[C_BYTES:[0-9]+]] = "llvm.ptrtoint"(%[[C_BYTES_PTR]]) : (!llvm.ptr) -> i64
// CHECK: %[[C_PTR:[0-9]+]] = llvm.call @malloc(%[[C_BYTES]]) : (i64) -> !llvm.ptr
// CHECK: ^bb{{[0-9]+}}(%[[P:[0-9]+]]: i64, %[[ACC:[0-9]+]]: f32):
// CHECK: %[[A_I_STRIDE:[0-9]+]] = "llvm.mul"(%{{[0-9]+}}, %[[K]]) : (i64, i64) -> i64
// CHECK: %[[A_P_STRIDE:[0-9]+]] = "llvm.mul"(%[[P]], %[[ONE]]) : (i64, i64) -> i64
// CHECK: %[[A_OFF0:[0-9]+]] = "llvm.add"(%[[ZERO]], %[[A_I_STRIDE]]) : (i64, i64) -> i64
// CHECK: %[[A_OFF:[0-9]+]] = "llvm.add"(%[[A_OFF0]], %[[A_P_STRIDE]]) : (i64, i64) -> i64
// CHECK: %[[A_ELEM:[0-9]+]] = "llvm.getelementptr"(%[[A_PTR]], %[[A_OFF]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[A_VAL:[0-9]+]] = llvm.load %[[A_ELEM]] : !llvm.ptr -> f32
// CHECK: %[[B_P_STRIDE:[0-9]+]] = "llvm.mul"(%[[P]], %[[M]]) : (i64, i64) -> i64
// CHECK: %[[B_J_STRIDE:[0-9]+]] = "llvm.mul"(%{{[0-9]+}}, %[[ONE]]) : (i64, i64) -> i64
// CHECK: %[[B_OFF0:[0-9]+]] = "llvm.add"(%[[ZERO]], %[[B_P_STRIDE]]) : (i64, i64) -> i64
// CHECK: %[[B_OFF:[0-9]+]] = "llvm.add"(%[[B_OFF0]], %[[B_J_STRIDE]]) : (i64, i64) -> i64
// CHECK: %[[B_ELEM:[0-9]+]] = "llvm.getelementptr"(%[[B_PTR]], %[[B_OFF]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[B_VAL:[0-9]+]] = llvm.load %[[B_ELEM]] : !llvm.ptr -> f32
// CHECK: %[[PROD:[0-9]+]] = "llvm.fmul"(%[[A_VAL]], %[[B_VAL]]) : (f32, f32) -> f32
// CHECK: %[[NEXT_ACC:[0-9]+]] = "llvm.fadd"(%[[ACC]], %[[PROD]]) : (f32, f32) -> f32
// CHECK: %[[C_I_STRIDE:[0-9]+]] = "llvm.mul"(%{{[0-9]+}}, %[[M]]) : (i64, i64) -> i64
// CHECK: %[[C_J_STRIDE:[0-9]+]] = "llvm.mul"(%{{[0-9]+}}, %[[ONE]]) : (i64, i64) -> i64
// CHECK: %[[C_OFF0:[0-9]+]] = "llvm.add"(%[[ZERO]], %[[C_I_STRIDE]]) : (i64, i64) -> i64
// CHECK: %[[C_OFF:[0-9]+]] = "llvm.add"(%[[C_OFF0]], %[[C_J_STRIDE]]) : (i64, i64) -> i64
// CHECK: %[[C_ELEM:[0-9]+]] = "llvm.getelementptr"(%[[C_PTR]], %[[C_OFF]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: "llvm.store"(%{{[0-9]+}}, %[[C_ELEM]]) : (f32, !llvm.ptr) -> ()
// CHECK: llvm.call @free(%[[A_PTR]]) : (!llvm.ptr) -> ()
// CHECK: llvm.call @free(%[[B_PTR]]) : (!llvm.ptr) -> ()
// CHECK: llvm.call @free(%[[C_PTR]]) : (!llvm.ptr) -> ()
// CHECK: "llvm.return"(%[[RET]]) : (i32) -> ()
