builtin.module {
  func.func @lifted_2(%0: i64) -> i64 {
    func.return %0 : i64
  }
  func.func @lifted_1(%0: i32) -> i32 {
    func.return %0 : i32
  }
  func.func @polymorphic_identity_specialization(%0: i32, %1: i64) -> i64 {
    %2 = "arith.extsi"(%0) : (i32) -> i64
    %3 = "arith.addi"(%2, %1) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %3 : i64
  }
}
