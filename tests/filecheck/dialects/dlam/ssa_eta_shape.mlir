// RUN: scair-opt -p=verify-type-params %s | filecheck %s --dump-input=fail

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
  }) : () -> !dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>
}

//CHECK: builtin.module { 
//CHECK:   %0 = "dlam.tlambda"() ({ 
//CHECK:   ^bb0(%1: !dlam.type): 
//CHECK:     %2 = "dlam.vlambda"() <{funAttr = 
//CHECK:       !dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>
//CHECK:     }> ({ 
//CHECK:     ^bb1(%3: !dlam.tvar<%1>): 
//CHECK:       "dlam.vreturn"(%3) 
//CHECK:         <{expected = !dlam.tvar<%1>}> 
//CHECK:         : (!dlam.tvar<%1>) -> () 
//CHECK:      }) : () -> !dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>
//CHECK:      "dlam.treturn"(%2) 
//CHECK:         <{expected = !dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>}> 
//CHECK:         : (!dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>) -> () 
//CHECK:     }) : () -> !dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>> 
//CHECK: }