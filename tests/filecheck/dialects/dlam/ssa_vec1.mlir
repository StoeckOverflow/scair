// RUN: scair-opt -a -p=verify-dependent-types %s | filecheck %s --dump-input=fail

builtin.module {
  %n = "dlam.nat_source"() : () -> i32
  %id = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.dep<vec<%n, i32>>, !dlam.dep<vec<%n, i32>>>}> ({
  ^bb0(%x: !dlam.dep<vec<%n, i32>>):
    "dlam.vreturn"(%x) <{expected = !dlam.dep<vec<%n, i32>>}>
      : (!dlam.dep<vec<%n, i32>>) -> ()
  }) : () -> !dlam.fun<!dlam.dep<vec<%n, i32>>, !dlam.dep<vec<%n, i32>>>
}

// CHECK: builtin.module

// First nat_source that defines the length
// CHECK: %[[N:[0-9]+]] = "dlam.nat_source"() : () -> i32

// The vlambda’s funAttr must use that same %[[N]]
// CHECK: %[[ID:[0-9]+]] = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.dep<vec<%[[N]], i32>>, !dlam.dep<vec<%[[N]], i32>>>}> ({
// CHECK: ^bb0(%[[X:[0-9]+]]: !dlam.dep<vec<%[[N]], i32>>):

// And vreturn must use the same vec<%[[N]], i32> in its expected type
// CHECK: "dlam.vreturn"(%[[X]]) <{expected = !dlam.dep<vec<%[[N]], i32>>}> : (!dlam.dep<vec<%[[N]], i32>>) -> ()

// CHECK: }) : () -> !dlam.fun<!dlam.dep<vec<%[[N]], i32>>, !dlam.dep<vec<%[[N]], i32>>>
