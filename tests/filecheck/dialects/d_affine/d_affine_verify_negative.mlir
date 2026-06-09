// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %bad = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%i)[] : (index)[] -> index
}

// VERIFY: d_affine.apply: expected 2 dim operands for map (d0, d1)[] -> (d0 + d1), got 1

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %bad = "d_affine.min"(%i) <{map = affine_map<(d0) -> (d0)>, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> i32
}

// VERIFY: d_affine.min: expected result type index, got i32

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %bad = d_affine.min affine_map<(d0)[s0] -> (d0 + s0)>(%i)[] : (index)[] -> index
}

// VERIFY: d_affine.min: expected 1 symbol operands for map (d0)[s0] -> (d0 + s0), got 0

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %bad = "d_affine.min"(%i) <{map = affine_map<(d0) -> ()>, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> index
}

// VERIFY: d_affine.min: expected at least one affine expression

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  d_affine.for %iv = affine_map<(d0) -> (d0, d0 + 1)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step 1 : i32 {
    d_affine.yield
  }
}

// VERIFY: d_affine.for: only single-result lower bound maps are supported, got 2 results

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0, d1) -> (d0 + d1)>(%ub) step 1 : i32 {
    d_affine.yield
  }
}

// VERIFY: d_affine.for: expected 2 upper bound operands for map (d0, d1)[] -> (d0 + d1), got 1

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  %s0 = "arith.constant"() <{value = 1 : index}> : () -> index
  %s1 = "arith.constant"() <{value = 2 : index}> : () -> index
  "d_affine.for"(%lb, %ub, %s0, %s1) <{
    lowerBoundMap = affine_map<(d0) -> (d0)>,
    upperBoundMap = affine_map<(d0) -> (d0)>,
    step = 1 : i32,
    operandSegmentSizes = array<i32: 1, 1, 2, 0>
  }> ({
  ^body(%iv: index):
    d_affine.yield
  }) : (index, index, index, index) -> ()
}

// VERIFY: d_affine.for: expected at most one dynamic step operand, got 2

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index
  %bad = "d_affine.for"(%lb, %ub, %init) <{
    lowerBoundMap = affine_map<(d0) -> (d0)>,
    upperBoundMap = affine_map<(d0) -> (d0)>,
    step = 1 : i32,
    operandSegmentSizes = array<i32: 1, 1, 0, 1>
  }> ({
  ^body(%iv: index, %acc: index):
    "test.no_terminator"() : () -> ()
  }) : (index, index, index) -> i32
}

// VERIFY: d_affine.for: init/result type mismatch at position 0; expected index, got i32

// -----

builtin.module {
  %lb = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "arith.constant"() <{value = 4 : index}> : () -> index
  "d_affine.for"(%lb, %ub) <{
    lowerBoundMap = affine_map<(d0) -> (d0)>,
    upperBoundMap = affine_map<(d0) -> (d0)>,
    step = 1 : i32,
    operandSegmentSizes = array<i32: 1, 1, 0, 0>
  }> ({
  ^body(%iv: i32):
    d_affine.yield
  }) : (index, index) -> ()
}

// VERIFY: d_affine.for: expected induction variable type index, got i32

// -----

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %j = "arith.constant"() <{value = 2 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  %bad = "d_affine.load"(%buf, %i) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (!d_memref.memref<[%m, %n], i32>, index) -> i32
}

// VERIFY: d_affine.load: expected 2 map operands for map (d0, d1)[] -> (d0, d1), got 1

// -----

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  %bad = "d_affine.load"(%buf, %i) <{map = affine_map<(d0) -> (d0)>}>
    : (!d_memref.memref<[%m, %n], i32>, index) -> i32
}

// VERIFY: d_affine.load: expected 2 map results for memref rank 2, got 1

// -----

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %j = "arith.constant"() <{value = 2 : index}> : () -> index
  %v = "arith.constant"() <{value = 7 : i64}> : () -> i64
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  "d_affine.store"(%v, %buf, %i, %j) <{map = affine_map<(d0, d1) -> (d0, d1)>}>
    : (i64, !d_memref.memref<[%m, %n], i32>, index, index) -> ()
}

// VERIFY: d_affine.store: expected stored value type i32, got i64

// -----

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], i32>
  "d_affine.store"(%v, %buf, %i) <{map = affine_map<(d0) -> (d0)>}>
    : (i32, !d_memref.memref<[%m, %n], i32>, index) -> ()
}

// VERIFY: d_affine.store: expected 2 map results for memref rank 2, got 1

// -----

builtin.module {
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %r = "d_affine.if"(%i) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
  ^then:
  }, {
  ^else:
    d_affine.yield %i : (index)
  }) : (index) -> index
}

// VERIFY: d_affine.if: expected non-empty then region terminated by d_affine.yield

// -----

builtin.module {
  %i = "arith.constant"() <{value = 1 : index}> : () -> index
  %v = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %r = "d_affine.if"(%i) <{condition = affine_set<(d0) : (d0 >= 0)>}> ({
  ^then:
    d_affine.yield %i : (index)
  }, {
  ^else:
    d_affine.yield %v : (i32)
  }) : (index) -> index
}

// VERIFY: d_affine.yield: operand type mismatch at position 0; expected index, got i32
