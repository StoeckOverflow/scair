// RUN: scair-opt -a -p=verify-dependent-types %s | filecheck %s --dump-input=fail

builtin.module {
  %n = "dlam.nat_source"() : () -> i32
  %m = "dlam.nat_source"() : () -> i32

  %f = "dlam.vlambda"() <{funAttr =
            !dlam.fun<
              !dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>,
              !dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>>
          }> ({
  ^bb0(%x: !dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>):
    "dlam.vreturn"(%x) <{expected = !dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>}>
      : (!dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>) -> ()
  }) : () -> !dlam.fun<
                !dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>,
                !dlam.dep<vec<%n + %m, !dlam.dep<vec<%m, i32>>>>>
}

// CHECK: builtin.module { 
// CHECK:  %0 = "dlam.nat_source"() : () -> i32 
// CHECK:  %1 = "dlam.nat_source"() : () -> i32 
// CHECK:  %2 = "dlam.vlambda"() <{funAttr = 
// CHECK:            !dlam.fun<
// CHECK:              !dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>, 
// CHECK:              !dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>>
// CHECK:              }> ({ 
// CHECK:  ^bb0(%3: !dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>): 
// CHECK:    "dlam.vreturn"(%3) <{expected = !dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>}> 
// CHECK:      : (!dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>) -> () 
// CHECK:  }) : () -> !dlam.fun<
// CHECK:                !dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>, 
// CHECK:                !dlam.dep<vec<%0 + %1, !dlam.dep<vec<%1, i32>>>>> 
// CHECK:} 
