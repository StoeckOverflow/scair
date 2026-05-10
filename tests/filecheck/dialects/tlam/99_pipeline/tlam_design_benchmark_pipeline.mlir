// Purpose: Thesis-facing TLam design benchmark pipeline smoke.
// Invariants covered: small identity and tensor-shaped polymorphic examples fully specialize, erase, and lower with no residual TLam constructs.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s --check-prefix=LOWER

builtin.module {
  func.func @polymorphic_identity_specialization(%i32v: i32, %i64v: i64) -> i64 {
    %id = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        "tlam.vreturn"(%x) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %id_i32 = "tlam.tapply"(%id) <{tyArg = i32}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> !tlam.fun<i32, i32>
    %id_i64 = "tlam.tapply"(%id) <{tyArg = i64}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> !tlam.fun<i64, i64>

    %r32 = "tlam.vapply"(%id_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r64 = "tlam.vapply"(%id_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r32_64 = "arith.extsi"(%r32) : (i32) -> i64
    %sum = "arith.addi"(%r32_64, %r64) : (i64, i64) -> i64
    func.return %sum : i64
  }
}

// -----

builtin.module {
  func.func @tensor_shape_identity(%x: tensor<4xi32>) -> tensor<4xi32> {
    %id = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%arg: !value<%T>):
        "tlam.vreturn"(%arg) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %spec = "tlam.tapply"(%id) <{tyArg = tensor<4xi32>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> !tlam.fun<tensor<4xi32>, tensor<4xi32>>
    %r = "tlam.vapply"(%spec, %x)
      : (!tlam.fun<tensor<4xi32>, tensor<4xi32>>, tensor<4xi32>)
        -> tensor<4xi32>
    func.return %r : tensor<4xi32>
  }
}

// LOWER-NOT: tlam
// LOWER: func.func @lifted_
// LOWER-NOT: tlam
// LOWER: func.func @polymorphic_identity_specialization
// LOWER-NOT: tlam
// LOWER: func.func @lifted_
// LOWER-NOT: tlam
// LOWER: func.func @tensor_shape_identity
// LOWER-NOT: tlam
