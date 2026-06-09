// RUN: scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-size-witnesses-to-index | filecheck %s

builtin.module {
  func.func @erase_nat_proofs(%k0: index, %out: memref<?xf32>) {
    %k0_size = "d_tensor.size.import"(%k0) : (index) -> !d_tensor.size
    %k1_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %k_size = "d_tensor.size.mul"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    "test.keep"(%k_size, %c0, %cst, %out) : (!d_tensor.size, index, f32, memref<?xf32>) -> ()

    "func.return"() : () -> ()
  }

  func.func @erase_nat_add(%a: index, %b: index) {
    %a_size = "d_tensor.size.import"(%a) : (index) -> !d_tensor.size
    %b_size = "d_tensor.size.import"(%b) : (index) -> !d_tensor.size
    %sum_size = "d_tensor.size.add"(%a_size, %b_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    "test.keep"(%sum_size) : (!d_tensor.size) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @erase_nat_proofs
// CHECK: %[[K1:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %[[K1]])
// CHECK: "test.keep"(%[[K]], %{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}) : (index, index, f32, memref<?xf32>) -> ()
// CHECK-LABEL: func.func @erase_nat_add
// CHECK: %[[SUM:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK: "test.keep"(%[[SUM]]) : (index) -> ()
// CHECK-NOT: d_tensor.
