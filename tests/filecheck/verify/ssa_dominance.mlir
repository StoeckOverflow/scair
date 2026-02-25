// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s -dump-input=always

// A passing case that exercises nested regions and multiple uses.
builtin.module {
  %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
  "test.use"(%0) : (i32) -> ()
  "test.op"() ({
    "test.use"(%0) : (i32) -> ()
    %1 = "arith.constant"() <{value = 2 : i32}> : () -> i32
    "test.use"(%1, %0) : (i32, i32) -> ()
  }) : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: [[C:%[0-9]+]] = "arith.constant"()
// CHECK: "test.use"([[C]])
// CHECK: "test.op"() ({
// CHECK: "test.use"([[C]])
// CHECK: [[D:%[0-9]+]] = "arith.constant"()
// CHECK: "test.use"([[D]], [[C]])
// CHECK: }) : () -> ()
// CHECK: }

// -----

// Use-before-def in the same block.
builtin.module {
  "test.use"(%0) : (i32) -> ()
  %0 = "arith.constant"() <{value = 0 : i32}> : () -> i32
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// CFG case where the definition dominates all paths (should pass).
builtin.module {
  "test.region"() ({
  ^bb0:
    %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    "test.use"(%x) : (i32) -> ()
    "test.br"()[^bb3] : () -> ()
  ^bb2:
    "test.use"(%x) : (i32) -> ()
    "test.br"()[^bb3] : () -> ()
  ^bb3:
    "test.use"(%x) : (i32) -> ()
    "test.ret"() : () -> ()
  }) : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: "test.region"() ({
// CHECK: [[X:%[0-9]+]] = "arith.constant"()
// CHECK: [[C:%[0-9]+]] = "arith.constant"()
// CHECK: "test.cond_br"([[C]])[^bb{{[0-9]+}}, ^bb{{[0-9]+}}] : (i1) -> ()
// CHECK: ^bb{{[0-9]+}}:
// CHECK: "test.use"([[X]]) : (i32) -> ()
// CHECK: "test.br"()[^bb{{[0-9]+}}] : () -> ()
// CHECK: ^bb{{[0-9]+}}:
// CHECK: "test.use"([[X]]) : (i32) -> ()
// CHECK: "test.br"()[^bb{{[0-9]+}}] : () -> ()
// CHECK: ^bb{{[0-9]+}}:
// CHECK: "test.use"([[X]]) : (i32) -> ()
// CHECK: "test.ret"() : () -> ()
// CHECK: }) : () -> ()
// CHECK: }

// -----

// CFG join where the def does not dominate the join (should fail).
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
    "test.br"()[^bb2] : () -> ()
  ^bb2:
    "test.use"(%x) : (i32) -> ()
    "test.ret"() : () -> ()
  }) : () -> ()
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// Block argument is only valid in its defining block (should fail).
builtin.module {
  "test.region"() ({
  ^bb0(%arg0: i32):
    "test.br"()[^bb1] : () -> ()
  ^bb1:
    "test.use"(%arg0) : (i32) -> ()
    "test.ret"() : () -> ()
  }) : () -> ()
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// Value used in a dependent result type before the defining op.
builtin.module {
  %1 = "arith.constant"() <{value = 1 : i32}> : () -> !value<%t0>
  %t0 = "test.make_type"() : () -> !tlam.type
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `arith.constant`

// -----

// Value used in an attribute before the defining op.
builtin.module {
  "test.use"() {dep = !value<%t0>} : () -> ()
  %t0 = "test.make_type"() : () -> !tlam.type
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`
