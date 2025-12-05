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

// CHECK: builtin.module

// Two nat sources
// CHECK: %[[N:[0-9]+]] = "dlam.nat_source"() : () -> i32
// CHECK: %[[M:[0-9]+]] = "dlam.nat_source"() : () -> i32

// funAttr uses %[[N]] + %[[M]] for the *outer* length (inner vec is still %m)
// CHECK: %[[F:[0-9]+]] = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.dep<vec<%[[N]] + %[[M]], !dlam.dep<vec<%m, i32>>>>, !dlam.dep<vec<%[[N]] + %[[M]], !dlam.dep<vec<%m, i32>>>>>}> ({

// Block arg has matching outer shape
// CHECK: ^bb0(%[[X:[0-9]+]]: !dlam.dep<vec<%[[N]] + %[[M]], !dlam.dep<vec<%m, i32>>>>):

// vreturn expected type matches the block arg type
// CHECK: "dlam.vreturn"(%[[X]]) <{expected = !dlam.dep<vec<%[[N]] + %[[M]], !dlam.dep<vec<%m, i32>>>>}> : (!dlam.dep<vec<%[[N]] + %[[M]], !dlam.dep<vec<%m, i32>>>>) -> ()
