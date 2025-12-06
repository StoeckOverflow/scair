// RUN: scair-opt -a -p=verify-dependent-types %s | filecheck %s --dump-input=fail --dump-input-filter=all

builtin.module {
  // F = ΛA. λ(x : vec<len, A>). x
  %len = "dlam.nat_source"() : () -> i32

  %F = "dlam.tlambda"() ({
  ^bb0(%A: !dlam.type):

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
           !dlam.dep<vec<%len, !dlam.bvar<0>>>,
           !dlam.dep<vec<%len, !dlam.bvar<0>>>
         >
       >
}

//CHECK: builtin.module { 
//CHECK:   %0 = "dlam.nat_source"() : () -> i32 
//CHECK:   %1 = "dlam.tlambda"() ({     
//CHECK:   ^bb0(%2: !dlam.type): 
//CHECK:     %3 = "dlam.vlambda"() <{funAttr = 
//CHECK:      !dlam.fun<
//CHECK:         !dlam.dep<vec<%0, !dlam.tvar<%2>>>, 
//CHECK:         !dlam.dep<vec<%0, !dlam.tvar<%2>>>>
//CHECK:       }> ({ 
//CHECK:       ^bb1(%4: !dlam.dep<vec<%0, !dlam.tvar<%2>>>): 
//CHECK:         "dlam.vreturn"(%4) 
//CHECK:         <{expected = !dlam.dep<vec<%0, !dlam.tvar<%2>>>}> 
//CHECK:         : (!dlam.dep<vec<%0, !dlam.tvar<%2>>>) -> () 
//CHECK:       }) : () -> !dlam.fun<
//CHECK:                  !dlam.dep<vec<%0, !dlam.tvar<%2>>>, 
//CHECK:                   !dlam.dep<vec<%0, !dlam.tvar<%2>>>
//CHECK:                   > 
//CHECK:         "dlam.treturn"(%3) 
//CHECK:           <{expected = !dlam.fun<
//CHECK:                         !dlam.dep<vec<%0, !dlam.tvar<%2>>>, 
//CHECK:                         !dlam.dep<vec<%0, !dlam.tvar<%2>>>
//CHECK:                         >
//CHECK:                       }> 
//CHECK:           : (!dlam.fun<
//CHECK:               !dlam.dep<vec<%0, !dlam.tvar<%2>>>, 
//CHECK:               !dlam.dep<vec<%0, !dlam.tvar<%2>>>
//CHECK:               >
//CHECK:             ) -> () 
//CHECK:           }) : () -> !dlam.forall<
//CHECK:                   !dlam.fun<
//CHECK:                     !dlam.dep<vec<%0, !dlam.bvar<0>>>, 
//CHECK:                     !dlam.dep<vec<%0, !dlam.bvar<0>>>>
//CHECK:                   > 
//CHECK: }