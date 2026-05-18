builtin.module {
  func.func @dependent_same_shape_different_ssa(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.tensor<[%0, %1], f32>, %3: !dtensor.tensor<[%0, %1], f32>, %4: !dtensor.tensor<[%0, %1], f32>, %5: !dtensor.tensor<[%0, %1], f32>, %6: !dtensor.tensor<[%0, %1], f32>, %7: !dtensor.tensor<[%0, %1], f32>) -> index {
    %8 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
    %9 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %10 = "arith.muli"(%8, %9) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %11 = "arith.addi"(%10, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %12 = "arith.addi"(%11, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %13 = "arith.addi"(%12, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %14 = "arith.addi"(%13, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %15 = "arith.addi"(%14, %10) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    func.return %15 : index
  }
}
