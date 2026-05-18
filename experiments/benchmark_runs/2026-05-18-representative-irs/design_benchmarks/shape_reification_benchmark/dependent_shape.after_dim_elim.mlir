builtin.module {
  func.func @dependent_same_shape_different_ssa(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.tensor<[%0, %1], f32>, %3: !dtensor.tensor<[%0, %1], f32>, %4: !dtensor.tensor<[%0, %1], f32>, %5: !dtensor.tensor<[%0, %1], f32>, %6: !dtensor.tensor<[%0, %1], f32>, %7: !dtensor.tensor<[%0, %1], f32>) -> index {
    %8 = "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
    %9 = "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
    %10 = "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
    %11 = "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
    %12 = "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
    %13 = "builtin.unrealized_conversion_cast"(%0) : (!dtensor.nat) -> !dtensor.nat
    %14 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat
    %15 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat
    %16 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat
    %17 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat
    %18 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat
    %19 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.nat) -> !dtensor.nat
    %20 = "dtensor.shape.to_index"(%8) : (!dtensor.nat) -> index
    %21 = "dtensor.shape.to_index"(%9) : (!dtensor.nat) -> index
    %22 = "dtensor.shape.to_index"(%10) : (!dtensor.nat) -> index
    %23 = "dtensor.shape.to_index"(%11) : (!dtensor.nat) -> index
    %24 = "dtensor.shape.to_index"(%12) : (!dtensor.nat) -> index
    %25 = "dtensor.shape.to_index"(%13) : (!dtensor.nat) -> index
    %26 = "dtensor.shape.to_index"(%14) : (!dtensor.nat) -> index
    %27 = "dtensor.shape.to_index"(%15) : (!dtensor.nat) -> index
    %28 = "dtensor.shape.to_index"(%16) : (!dtensor.nat) -> index
    %29 = "dtensor.shape.to_index"(%17) : (!dtensor.nat) -> index
    %30 = "dtensor.shape.to_index"(%18) : (!dtensor.nat) -> index
    %31 = "dtensor.shape.to_index"(%19) : (!dtensor.nat) -> index
    %32 = "arith.muli"(%20, %26) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %33 = "arith.muli"(%21, %27) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %34 = "arith.muli"(%22, %28) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %35 = "arith.muli"(%23, %29) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %36 = "arith.muli"(%24, %30) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %37 = "arith.muli"(%25, %31) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %38 = "arith.addi"(%32, %33) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %39 = "arith.addi"(%38, %34) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %40 = "arith.addi"(%39, %35) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %41 = "arith.addi"(%40, %36) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %42 = "arith.addi"(%41, %37) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    func.return %42 : index
  }
}
