builtin.module {
  func.func @lifted_48(%0: f64) -> f64 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %2 = "arith.addf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %3 = "arith.mulf"(%2, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %3 : f64
  }
  func.func @lifted_47(%0: f64) -> f64 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %2 = "arith.addf"(%1, %1) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %2 : f64
  }
  func.func @lifted_46(%0: f64) -> f64 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %2 = "arith.mulf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %2 : f64
  }
  func.func @lifted_45(%0: f64) -> f64 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %2 = "arith.mulf"(%1, %1) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %2 : f64
  }
  func.func @lifted_44(%0: f64) -> f64 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %2 = "arith.addf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %2 : f64
  }
  func.func @lifted_43(%0: f64) -> f64 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %2 = "arith.addf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %2 : f64
  }
  func.func @lifted_42(%0: f64) -> f64 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %1 : f64
  }
  func.func @lifted_41(%0: f64) -> f64 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    func.return %1 : f64
  }
  func.func @lifted_40(%0: f32) -> f32 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %2 = "arith.addf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %3 = "arith.mulf"(%2, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %3 : f32
  }
  func.func @lifted_39(%0: f32) -> f32 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %2 = "arith.addf"(%1, %1) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %2 : f32
  }
  func.func @lifted_38(%0: f32) -> f32 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %2 = "arith.mulf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %2 : f32
  }
  func.func @lifted_37(%0: f32) -> f32 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %2 = "arith.mulf"(%1, %1) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %2 : f32
  }
  func.func @lifted_36(%0: f32) -> f32 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %2 = "arith.addf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %2 : f32
  }
  func.func @lifted_35(%0: f32) -> f32 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %2 = "arith.addf"(%1, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %2 : f32
  }
  func.func @lifted_34(%0: f32) -> f32 {
    %1 = "arith.mulf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %1 : f32
  }
  func.func @lifted_33(%0: f32) -> f32 {
    %1 = "arith.addf"(%0, %0) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    func.return %1 : f32
  }
  func.func @lifted_32(%0: i64) -> i64 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %3 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %3 : i64
  }
  func.func @lifted_31(%0: i64) -> i64 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %2 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %2 : i64
  }
  func.func @lifted_30(%0: i64) -> i64 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %2 = "arith.muli"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %2 : i64
  }
  func.func @lifted_29(%0: i64) -> i64 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %2 = "arith.muli"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %2 : i64
  }
  func.func @lifted_28(%0: i64) -> i64 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %2 : i64
  }
  func.func @lifted_27(%0: i64) -> i64 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %2 : i64
  }
  func.func @lifted_26(%0: i64) -> i64 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %1 : i64
  }
  func.func @lifted_25(%0: i64) -> i64 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %1 : i64
  }
  func.func @lifted_24(%0: i32) -> i32 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %3 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %3 : i32
  }
  func.func @lifted_23(%0: i32) -> i32 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %2 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %2 : i32
  }
  func.func @lifted_22(%0: i32) -> i32 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %2 = "arith.muli"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %2 : i32
  }
  func.func @lifted_21(%0: i32) -> i32 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %2 = "arith.muli"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %2 : i32
  }
  func.func @lifted_20(%0: i32) -> i32 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %2 : i32
  }
  func.func @lifted_19(%0: i32) -> i32 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %2 : i32
  }
  func.func @lifted_18(%0: i32) -> i32 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %1 : i32
  }
  func.func @lifted_17(%0: i32) -> i32 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    func.return %1 : i32
  }
  func.func @lifted_16(%0: i16) -> i16 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %3 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %3 : i16
  }
  func.func @lifted_15(%0: i16) -> i16 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %2 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %2 : i16
  }
  func.func @lifted_14(%0: i16) -> i16 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %2 = "arith.muli"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %2 : i16
  }
  func.func @lifted_13(%0: i16) -> i16 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %2 = "arith.muli"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %2 : i16
  }
  func.func @lifted_12(%0: i16) -> i16 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %2 : i16
  }
  func.func @lifted_11(%0: i16) -> i16 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %2 : i16
  }
  func.func @lifted_10(%0: i16) -> i16 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %1 : i16
  }
  func.func @lifted_9(%0: i16) -> i16 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    func.return %1 : i16
  }
  func.func @lifted_8(%0: i8) -> i8 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %3 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %3 : i8
  }
  func.func @lifted_7(%0: i8) -> i8 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %2 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %2 : i8
  }
  func.func @lifted_6(%0: i8) -> i8 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %2 = "arith.muli"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %2 : i8
  }
  func.func @lifted_5(%0: i8) -> i8 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %2 = "arith.muli"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %2 : i8
  }
  func.func @lifted_4(%0: i8) -> i8 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %2 : i8
  }
  func.func @lifted_3(%0: i8) -> i8 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %2 = "arith.addi"(%1, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %2 : i8
  }
  func.func @lifted_2(%0: i8) -> i8 {
    %1 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %1 : i8
  }
  func.func @lifted_1(%0: i8) -> i8 {
    %1 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    func.return %1 : i8
  }
  func.func @shared_polymorphic_kernel_bank_multitype(%0: i8, %1: i16, %2: i32, %3: i64, %4: f32, %5: f64) -> i64 {
    %6 = "arith.addi"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %7 = "arith.muli"(%0, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %8 = "arith.addi"(%6, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %9 = "arith.addi"(%7, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %10 = "arith.muli"(%6, %6) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %11 = "arith.muli"(%7, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %12 = "arith.addi"(%7, %7) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %13 = "arith.muli"(%8, %0) <{overflowFlags = #arith.overflow<none>}> : (i8, i8) -> i8
    %14 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %15 = "arith.muli"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %16 = "arith.addi"(%14, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %17 = "arith.addi"(%15, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %18 = "arith.muli"(%14, %14) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %19 = "arith.muli"(%15, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %20 = "arith.addi"(%15, %15) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %21 = "arith.muli"(%16, %1) <{overflowFlags = #arith.overflow<none>}> : (i16, i16) -> i16
    %22 = "arith.addi"(%2, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %23 = "arith.muli"(%2, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %24 = "arith.addi"(%22, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %25 = "arith.addi"(%23, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %26 = "arith.muli"(%22, %22) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %27 = "arith.muli"(%23, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %28 = "arith.addi"(%23, %23) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %29 = "arith.muli"(%24, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
    %30 = "arith.addi"(%3, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %31 = "arith.muli"(%3, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %32 = "arith.addi"(%30, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %33 = "arith.addi"(%31, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %34 = "arith.muli"(%30, %30) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %35 = "arith.muli"(%31, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %36 = "arith.addi"(%31, %31) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %37 = "arith.muli"(%32, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %38 = "arith.addf"(%4, %4) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %39 = "arith.mulf"(%4, %4) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %40 = "arith.addf"(%38, %4) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %41 = "arith.addf"(%39, %4) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %42 = "arith.mulf"(%38, %38) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %43 = "arith.mulf"(%39, %4) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %44 = "arith.addf"(%39, %39) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %45 = "arith.mulf"(%40, %4) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %46 = "arith.addf"(%5, %5) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %47 = "arith.mulf"(%5, %5) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %48 = "arith.addf"(%46, %5) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %49 = "arith.addf"(%47, %5) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %50 = "arith.mulf"(%46, %46) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %51 = "arith.mulf"(%47, %5) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %52 = "arith.addf"(%47, %47) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %53 = "arith.mulf"(%48, %5) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %54 = "arith.extsi"(%6) : (i8) -> i64
    %55 = "arith.extsi"(%7) : (i8) -> i64
    %56 = "arith.extsi"(%8) : (i8) -> i64
    %57 = "arith.extsi"(%9) : (i8) -> i64
    %58 = "arith.extsi"(%10) : (i8) -> i64
    %59 = "arith.extsi"(%11) : (i8) -> i64
    %60 = "arith.extsi"(%12) : (i8) -> i64
    %61 = "arith.extsi"(%13) : (i8) -> i64
    %62 = "arith.extsi"(%14) : (i16) -> i64
    %63 = "arith.extsi"(%15) : (i16) -> i64
    %64 = "arith.extsi"(%16) : (i16) -> i64
    %65 = "arith.extsi"(%17) : (i16) -> i64
    %66 = "arith.extsi"(%18) : (i16) -> i64
    %67 = "arith.extsi"(%19) : (i16) -> i64
    %68 = "arith.extsi"(%20) : (i16) -> i64
    %69 = "arith.extsi"(%21) : (i16) -> i64
    %70 = "arith.extsi"(%22) : (i32) -> i64
    %71 = "arith.extsi"(%23) : (i32) -> i64
    %72 = "arith.extsi"(%24) : (i32) -> i64
    %73 = "arith.extsi"(%25) : (i32) -> i64
    %74 = "arith.extsi"(%26) : (i32) -> i64
    %75 = "arith.extsi"(%27) : (i32) -> i64
    %76 = "arith.extsi"(%28) : (i32) -> i64
    %77 = "arith.extsi"(%29) : (i32) -> i64
    %78 = "arith.fptosi"(%38) : (f32) -> i64
    %79 = "arith.fptosi"(%39) : (f32) -> i64
    %80 = "arith.fptosi"(%40) : (f32) -> i64
    %81 = "arith.fptosi"(%41) : (f32) -> i64
    %82 = "arith.fptosi"(%42) : (f32) -> i64
    %83 = "arith.fptosi"(%43) : (f32) -> i64
    %84 = "arith.fptosi"(%44) : (f32) -> i64
    %85 = "arith.fptosi"(%45) : (f32) -> i64
    %86 = "arith.fptosi"(%46) : (f64) -> i64
    %87 = "arith.fptosi"(%47) : (f64) -> i64
    %88 = "arith.fptosi"(%48) : (f64) -> i64
    %89 = "arith.fptosi"(%49) : (f64) -> i64
    %90 = "arith.fptosi"(%50) : (f64) -> i64
    %91 = "arith.fptosi"(%51) : (f64) -> i64
    %92 = "arith.fptosi"(%52) : (f64) -> i64
    %93 = "arith.fptosi"(%53) : (f64) -> i64
    %94 = "arith.addi"(%54, %55) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %95 = "arith.addi"(%94, %56) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %96 = "arith.addi"(%95, %57) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %97 = "arith.addi"(%96, %58) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %98 = "arith.addi"(%97, %59) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %99 = "arith.addi"(%98, %60) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %100 = "arith.addi"(%99, %61) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %101 = "arith.addi"(%100, %62) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %102 = "arith.addi"(%101, %63) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %103 = "arith.addi"(%102, %64) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %104 = "arith.addi"(%103, %65) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %105 = "arith.addi"(%104, %66) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %106 = "arith.addi"(%105, %67) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %107 = "arith.addi"(%106, %68) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %108 = "arith.addi"(%107, %69) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %109 = "arith.addi"(%108, %70) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %110 = "arith.addi"(%109, %71) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %111 = "arith.addi"(%110, %72) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %112 = "arith.addi"(%111, %73) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %113 = "arith.addi"(%112, %74) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %114 = "arith.addi"(%113, %75) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %115 = "arith.addi"(%114, %76) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %116 = "arith.addi"(%115, %77) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %117 = "arith.addi"(%116, %30) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %118 = "arith.addi"(%117, %31) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %119 = "arith.addi"(%118, %32) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %120 = "arith.addi"(%119, %33) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %121 = "arith.addi"(%120, %34) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %122 = "arith.addi"(%121, %35) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %123 = "arith.addi"(%122, %36) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %124 = "arith.addi"(%123, %37) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %125 = "arith.addi"(%124, %78) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %126 = "arith.addi"(%125, %79) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %127 = "arith.addi"(%126, %80) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %128 = "arith.addi"(%127, %81) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %129 = "arith.addi"(%128, %82) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %130 = "arith.addi"(%129, %83) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %131 = "arith.addi"(%130, %84) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %132 = "arith.addi"(%131, %85) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %133 = "arith.addi"(%132, %86) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %134 = "arith.addi"(%133, %87) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %135 = "arith.addi"(%134, %88) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %136 = "arith.addi"(%135, %89) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %137 = "arith.addi"(%136, %90) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %138 = "arith.addi"(%137, %91) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %139 = "arith.addi"(%138, %92) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %140 = "arith.addi"(%139, %93) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %140 : i64
  }
}
