// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 8 : index}> : () -> index
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 {
    %m = d_affine.min affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%ub] : (index)[index] -> index
    "test.keep"(%m) : (index) -> ()
    d_affine.yield
  }
}

// VERIFY: d_affine.for %{{.*}} = #{{.*}}(%{{.*}}) to #{{.*}}(%{{.*}}) step 1 : i32 {
// VERIFY: %{{.*}} = d_affine.min #{{.*}}(%{{.*}})[%{{.*}}] : (index)[index] -> index
// VERIFY: d_affine.yield

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 8 : index}> : () -> index
  %step = "arith.constant"() <{value = 2 : index}> : () -> index
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
    d_affine.yield
  }
}

// VERIFY: d_affine.for %{{.*}} = #{{.*}}(%{{.*}}) to #{{.*}}(%{{.*}}) step %{{.*}} : index {
// VERIFY: d_affine.yield

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %r = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%i)[%i] : (index)[index] -> index
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: #map = affine_map<(d0)[s0] -> (d0 + s0)>
// VERIFY: d_affine.apply #map (%{{.*}})[%{{.*}}]

// -----

builtin.module {
  d_affine.yield
}

// VERIFY: d_affine.yield: expected parent op d_affine.for

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 8 : index}> : () -> index
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 0 : i32 {
    d_affine.yield
  }
}

// VERIFY: d_affine.for: expected positive step, got 0

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 8 : index}> : () -> index
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 {
    "test.no_terminator"() : () -> ()
  }
}

// VERIFY: d_affine.for: expected terminator d_affine.yield

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %r = d_affine.apply affine_map<(d0) -> (d0, d0 + 1)>(%i)[] : (index)[] -> index
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: d_affine.apply: only single-result affine maps are supported, got 2 results

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %r = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%i)[] : (index)[] -> index
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: d_affine.apply: expected 1 symbol operands for map (d0)[s0] -> (d0 + s0), got 0

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %r = "d_affine.apply"(%i, %i) <{map = affine_map<(d0)[s0] -> (d0 + s0)>, operandSegmentSizes = array<i32: 1, 1>}> : (index, index) -> i32
  "test.keep"(%r) : (i32) -> ()
}

// VERIFY: d_affine.apply: expected result type index, got i32

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index
  %r = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
    d_affine.yield %iv : (index)
  }
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: d_affine.for %{{.*}} = #{{.*}}(%{{.*}}) to #{{.*}}(%{{.*}}) step 1 : i32 iter_args(%{{.*}} = %{{.*}} : index) {
// VERIFY: d_affine.yield %{{.*}} : (index)

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index
  %r = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
    d_affine.yield
  }
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: d_affine.yield: expected 1 operands to match parent results, got 0

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %r = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 iter_args(%acc = %init : index) {
    d_affine.yield %v : (i32)
  }
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: d_affine.yield: operand type mismatch at position 0; expected index, got i32

// -----

builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %j = "arith.constant"() <{value = 2 : index}> : () -> index
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  "d_affine.store"(%v, %buf, %i, %j) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (i32, !d_memref.memref<[%m, %n], i32>, index, index) -> ()
  %r = "d_affine.load"(%buf, %i, %j) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (!d_memref.memref<[%m, %n], i32>, index, index) -> i32
  "test.keep"(%r) : (i32) -> ()
}

// VERIFY: "d_affine.store"
// VERIFY: "d_affine.load"

// -----

builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %j = "arith.constant"() <{value = 2 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  %bad = "d_affine.load"(%buf, %i, %j) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (!d_memref.memref<[%m, %n], i32>, index, index) -> i64
  "test.keep"(%bad) : (i64) -> ()
}

// VERIFY: d_affine.load: expected result type i32, got i64

// -----

builtin.module {
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  "d_affine.if"(%i) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
  ^0:
  }, {
  ^1:
  }) : (index) -> ()

  "d_affine.parallel"(%i) <{
    lowerBoundsMap = affine_map<()[s0] -> (0)>,
    lowerBoundsGroups = dense<1> : vector<1xi32>,
    upperBoundsMap = affine_map<()[s0] -> (s0)>,
    upperBoundsGroups = dense<1> : vector<1xi32>,
    steps = [1 : i64],
    reductions = []
  }> ({
  ^2(%p: index):
  }) : (index) -> ()
}

// VERIFY: "d_affine.if"
// VERIFY: "d_affine.parallel"
