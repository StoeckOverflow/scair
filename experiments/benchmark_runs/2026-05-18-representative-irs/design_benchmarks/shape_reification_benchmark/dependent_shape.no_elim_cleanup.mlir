builtin.module {
  func.func @dependent_same_shape_different_ssa(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.tensor<[%0, %1], f32>, %3: !dtensor.tensor<[%0, %1], f32>, %4: !dtensor.tensor<[%0, %1], f32>, %5: !dtensor.tensor<[%0, %1], f32>, %6: !dtensor.tensor<[%0, %1], f32>, %7: !dtensor.tensor<[%0, %1], f32>) -> index {
    %8 = "dtensor.dim"(%2) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
    %9 = "dtensor.dim"(%3) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
    %10 = "dtensor.dim"(%4) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
    %11 = "dtensor.dim"(%5) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
    %12 = "dtensor.dim"(%6) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
    %13 = "dtensor.dim"(%7) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
    %14 = "dtensor.dim"(%2) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
    %15 = "dtensor.dim"(%3) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
    %16 = "dtensor.dim"(%4) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
    %17 = "dtensor.dim"(%5) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
    %18 = "dtensor.dim"(%6) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
    %19 = "dtensor.dim"(%7) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
    %20 = "builtin.unrealized_conversion_cast"(%8) : (!value<%0>) -> !dtensor.nat
    %21 = "builtin.unrealized_conversion_cast"(%9) : (!value<%0>) -> !dtensor.nat
    %22 = "builtin.unrealized_conversion_cast"(%10) : (!value<%0>) -> !dtensor.nat
    %23 = "builtin.unrealized_conversion_cast"(%11) : (!value<%0>) -> !dtensor.nat
    %24 = "builtin.unrealized_conversion_cast"(%12) : (!value<%0>) -> !dtensor.nat
    %25 = "builtin.unrealized_conversion_cast"(%13) : (!value<%0>) -> !dtensor.nat
    %26 = "builtin.unrealized_conversion_cast"(%14) : (!value<%1>) -> !dtensor.nat
    %27 = "builtin.unrealized_conversion_cast"(%15) : (!value<%1>) -> !dtensor.nat
    %28 = "builtin.unrealized_conversion_cast"(%16) : (!value<%1>) -> !dtensor.nat
    %29 = "builtin.unrealized_conversion_cast"(%17) : (!value<%1>) -> !dtensor.nat
    %30 = "builtin.unrealized_conversion_cast"(%18) : (!value<%1>) -> !dtensor.nat
    %31 = "builtin.unrealized_conversion_cast"(%19) : (!value<%1>) -> !dtensor.nat
    %32 = "dtensor.shape.to_index"(%20) : (!dtensor.nat) -> index
    %33 = "dtensor.shape.to_index"(%21) : (!dtensor.nat) -> index
    %34 = "dtensor.shape.to_index"(%22) : (!dtensor.nat) -> index
    %35 = "dtensor.shape.to_index"(%23) : (!dtensor.nat) -> index
    %36 = "dtensor.shape.to_index"(%24) : (!dtensor.nat) -> index
    %37 = "dtensor.shape.to_index"(%25) : (!dtensor.nat) -> index
    %38 = "dtensor.shape.to_index"(%26) : (!dtensor.nat) -> index
    %39 = "dtensor.shape.to_index"(%27) : (!dtensor.nat) -> index
    %40 = "dtensor.shape.to_index"(%28) : (!dtensor.nat) -> index
    %41 = "dtensor.shape.to_index"(%29) : (!dtensor.nat) -> index
    %42 = "dtensor.shape.to_index"(%30) : (!dtensor.nat) -> index
    %43 = "dtensor.shape.to_index"(%31) : (!dtensor.nat) -> index
    %44 = "arith.muli"(%32, %38) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %45 = "arith.muli"(%33, %39) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %46 = "arith.muli"(%34, %40) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %47 = "arith.muli"(%35, %41) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %48 = "arith.muli"(%36, %42) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %49 = "arith.muli"(%37, %43) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %50 = "arith.addi"(%44, %45) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %51 = "arith.addi"(%50, %46) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %52 = "arith.addi"(%51, %47) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %53 = "arith.addi"(%52, %48) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %54 = "arith.addi"(%53, %49) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    func.return %54 : index
  }
}
