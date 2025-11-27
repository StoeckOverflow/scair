// RUN: scair-opt -p=verify-dependent-types %s | filecheck %s --dump-input=fail

builtin.module {
  %F = "dlam.tlambda"() ({
  ^bb0(%T: !dlam.type):

    // η-shape: λx. x
    %id = "dlam.vlambda"() <{funAttr =
      !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>
    }> ({
    ^bb1(%x: !dlam.tvar<%T>):
      "dlam.vreturn"(%x)
        <{expected = !dlam.tvar<%T>}>
        : (!dlam.tvar<%T>) -> ()
    }) : () -> !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>

    "dlam.treturn"(%id)
      <{expected = !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>}>
      : (!dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>) -> ()
  }) : () -> !dlam.forall<!dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>>
}

// We don't care about concrete SSA numbers, just structure and sharing:
//  - a tlambda with a type binder T
//  - a vlambda with funAttr !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>
//  - a block arg x : !dlam.tvar<%T>
//  - vreturn x with expected = !dlam.tvar<%T>
//  - treturn id with expected = !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>

// CHECK: %[[F:[0-9]+]] = "dlam.tlambda"() ({
// CHECK: ^bb0(%[[T:[0-9]+]]: !dlam.type):

// CHECK: %[[ID:[0-9]+]] = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.tvar<%[[T]]>, !dlam.tvar<%[[T]]>>}> ({
// CHECK: ^bb{{[0-9]+}}(%[[X:[0-9]+]]: !dlam.tvar<%[[T]]>):
// CHECK: "dlam.vreturn"(%[[X]]) <{expected = !dlam.tvar<%[[T]]>}> : (!dlam.tvar<%[[T]]>) -> ()
// CHECK: }) : () -> !dlam.fun<!dlam.tvar<%[[T]]>, !dlam.tvar<%[[T]]>

// CHECK: "dlam.treturn"(%[[ID]]) <{expected = !dlam.fun<!dlam.tvar<%[[T]]>, !dlam.tvar<%[[T]]>>}>
// CHECK: }) : () -> !dlam.forall<!dlam.fun<!dlam.tvar<%[[T]]>, !dlam.tvar<%[[T]]>>>
