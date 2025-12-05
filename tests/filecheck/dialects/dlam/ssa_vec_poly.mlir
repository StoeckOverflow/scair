// RUN: scair-opt -a -p=verify-dependent-types %s | filecheck %s --dump-input=fail --dump-input-filter=all

builtin.module {
  // F = ΛA. λ(x : vec<len, A>). x
  %F = "dlam.tlambda"() ({
  ^bb0(%A: !dlam.type):

    %len = "dlam.nat_source"() : () -> i32

    %V = "dlam.vlambda"() <{funAttr =
      !dlam.fun<
        !dlam.dep<vec<%len, !dlam.tvar<%A>>>,
        !dlam.dep<vec<%len, !dlam.tvar<%A>>>
      >
    }> ({
    ^bb1(%x: !dlam.dep<vec<%len, !dlam.tvar<%A>>>):
      "dlam.vreturn"(%x)
        <{expected = !dlam.dep<vec<%len, !dlam.tvar<%A>>>}>
        : (!dlam.dep<vec<%len, !dlam.tvar<%A>>>) -> ()
    }) : () -> !dlam.fun<
           !dlam.dep<vec<%len, !dlam.tvar<%A>>>,
           !dlam.dep<vec<%len, !dlam.tvar<%A>>>
         >

    "dlam.treturn"(%V)
      <{expected =
        !dlam.fun<
          !dlam.dep<vec<%len, !dlam.tvar<%A>>>,
          !dlam.dep<vec<%len, !dlam.tvar<%A>>>
        >
      }>
      : (!dlam.fun<
          !dlam.dep<vec<%len, !dlam.tvar<%A>>>,
          !dlam.dep<vec<%len, !dlam.tvar<%A>>>
        >
        ) -> ()
  }) : () -> !dlam.forall<
         !dlam.fun<
           !dlam.dep<vec<%len, !dlam.tvar<%A>>>,
           !dlam.dep<vec<%len, !dlam.tvar<%A>>>
         >
       >
}

// Basic module + outer tlambda
// CHECK: builtin.module
// CHECK: %[[TL:[0-9]+]] = "dlam.tlambda"() ({
// CHECK: ^bb0(%[[AARG:[0-9]+]]: !dlam.type):

// Length source (we'll refer to it as [[LEN]])
// CHECK: %[[LEN:[0-9]+]] = "dlam.nat_source"() : () -> i32

// vlambda: funAttr uses vec<%[[LEN]], !dlam.tvar<%A>> in both domain and codomain
// NOTE: !dlam.tvar still prints %A textually, so we don't bind [[A]] here.
// CHECK: %[[V:[0-9]+]] = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>, !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>>}> ({

// Block arg type uses the same LEN and A
// CHECK: ^bb1(%[[X:[0-9]+]]: !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>):

// vreturn expected type and result type match vec<LEN, tvar<A>>
// CHECK: "dlam.vreturn"(%[[X]]) <{expected = !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>}> : (!dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>) -> ()

// vlambda result type
// CHECK: }) : () -> !dlam.fun<!dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>, !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>>

// treturn expected type and result also use the same LEN and %A
// CHECK: "dlam.treturn"(%[[V]]) <{expected = !dlam.fun<!dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>, !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>>}> : (!dlam.fun<!dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>, !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>>) -> ()

// forall result type at the end
// CHECK: }) : () -> !dlam.forall<!dlam.fun<!dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>, !dlam.dep<vec<%[[LEN]], !dlam.tvar<%A>>>>>
