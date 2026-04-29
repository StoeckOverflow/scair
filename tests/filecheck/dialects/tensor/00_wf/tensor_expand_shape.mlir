// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: one direct nat.mul-backed dimension split.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %q4 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
  "test.keep"(%q4) : (!dtensor.tensor<[%b, %s, %heads, %head_dim], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "test.q"() : () -> !dtensor.tensor<[%0, %1, %4], f32>
// CHECK-NEXT:   %6 = "dtensor.expand_shape"(%5) <{reassociation = {{\[\[0 : i32\], \[1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1, %4], f32>) -> !dtensor.tensor<[%0, %1, %2, %3], f32>
// CHECK-NEXT:   "test.keep"(%6) : (!dtensor.tensor<[%0, %1, %2, %3], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: rank-preserving no-op reassociation.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %hidden], f32>
  "test.keep"(%bad) : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "test.q"() : () -> !dtensor.tensor<[%0, %1, %4], f32>
// CHECK-NEXT:   %6 = "dtensor.expand_shape"(%5) <{reassociation = {{\[\[0 : i32\], \[1 : i32\], \[2 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1, %4], f32>) -> !dtensor.tensor<[%0, %1, %4], f32>
// CHECK-NEXT:   "test.keep"(%6) : (!dtensor.tensor<[%0, %1, %4], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: element type must be preserved.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], i32>
}

// CHECK: dtensor.expand_shape: expected equal element types

// -----

// Invalid: source dim must equal the ordered product of reassociated result dims.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: expected source dim 2 to equal ordered product of result dims [2, 3]

// -----

// Invalid: ordered product equality rejects commuted factors.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %head_dim, %heads], f32>
}

// CHECK: dtensor.expand_shape: expected source dim 2 to equal ordered product of result dims [2, 3]

// -----

// Invalid: product factor must match.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %other], f32>
}

// CHECK: dtensor.expand_shape: expected source dim 2 to equal ordered product of result dims [2, 3]

// -----

// Invalid: singleton groups use the same product equality path.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %b_other = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b_other, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: expected source dim 0 to equal ordered product of result dims [0]

// -----

// Invalid: reassociation must be contiguous and complete.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [2 : i32], [1 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Valid: multiple split groups.
builtin.module {
  %b0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %b1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %s0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %s1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "dtensor.nat.mul"(%b0, %b1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %hidden], f32>) -> !dtensor.tensor<[%b0, %b1, %heads, %head_dim], f32>
  "test.keep"(%bad) : (!dtensor.tensor<[%b0, %b1, %heads, %head_dim], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %5 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %6 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %7 = "dtensor.nat.mul"(%4, %5) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %8 = "test.q"() : () -> !dtensor.tensor<[%6, %7], f32>
// CHECK-NEXT:   %9 = "dtensor.expand_shape"(%8) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!dtensor.tensor<[%6, %7], f32>) -> !dtensor.tensor<[%0, %1, %4, %5], f32>
// CHECK-NEXT:   "test.keep"(%9) : (!dtensor.tensor<[%0, %1, %4, %5], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: one source dim can expand into more than two ordered factors.
builtin.module {
  %a = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "dtensor.nat.param"() : () -> !dtensor.nat
  %ab = "dtensor.nat.mul"(%a, %b) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %abc = "dtensor.nat.mul"(%ab, %c) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%abc], f32>
  %q3 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32, 2 : i32]]}>
    : (!dtensor.tensor<[%abc], f32>) -> !dtensor.tensor<[%a, %b, %c], f32>
  "test.keep"(%q3) : (!dtensor.tensor<[%a, %b, %c], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%3, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "test.q"() : () -> !dtensor.tensor<[%4], f32>
// CHECK-NEXT:   %6 = "dtensor.expand_shape"(%5) <{reassociation = {{\[\[0 : i32, 1 : i32, 2 : i32\]\]}}}> : (!dtensor.tensor<[%4], f32>) -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   "test.keep"(%6) : (!dtensor.tensor<[%0, %1, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: right-nested NatMul is normalized to the same ordered product.
builtin.module {
  %a = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "dtensor.nat.param"() : () -> !dtensor.nat
  %bc = "dtensor.nat.mul"(%b, %c) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %abc = "dtensor.nat.mul"(%a, %bc) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%abc], f32>
  %q3 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32, 2 : i32]]}>
    : (!dtensor.tensor<[%abc], f32>) -> !dtensor.tensor<[%a, %b, %c], f32>
  "test.keep"(%q3) : (!dtensor.tensor<[%a, %b, %c], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%0, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "test.q"() : () -> !dtensor.tensor<[%4], f32>
// CHECK-NEXT:   %6 = "dtensor.expand_shape"(%5) <{reassociation = {{\[\[0 : i32, 1 : i32, 2 : i32\]\]}}}> : (!dtensor.tensor<[%4], f32>) -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   "test.keep"(%6) : (!dtensor.tensor<[%0, %1, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: unit dimensions may be inserted around a symbolic factor.
builtin.module {
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %one = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%n], f32>
  %leading = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%n], f32>) -> !dtensor.tensor<[%one, %n], f32>
  %trailing = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%n], f32>) -> !dtensor.tensor<[%n, %one], f32>
  "test.keep"(%leading, %trailing) : (!dtensor.tensor<[%one, %n], f32>, !dtensor.tensor<[%n, %one], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "test.q"() : () -> !dtensor.tensor<[%0], f32>
// CHECK-NEXT:   %3 = "dtensor.expand_shape"(%2) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%1, %0], f32>
// CHECK-NEXT:   %4 = "dtensor.expand_shape"(%2) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%3, %4) : (!dtensor.tensor<[%1, %0], f32>, !dtensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: literal factors are folded in product equality.
builtin.module {
  %two = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %three = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %six = "dtensor.nat.const"() <{value = 6 : i32}> : () -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%six], f32>
  %q2 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%six], f32>) -> !dtensor.tensor<[%two, %three], f32>
  "test.keep"(%q2) : (!dtensor.tensor<[%two, %three], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.const"() <{value = 6 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.q"() : () -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   %4 = "dtensor.expand_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%2], f32>) -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: expand_shape cannot reduce rank.
builtin.module {
  %a = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %ab = "dtensor.nat.mul"(%a, %b) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%a, %b], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32]]}>
    : (!dtensor.tensor<[%a, %b], f32>) -> !dtensor.tensor<[%ab], f32>
}

// CHECK: dtensor.expand_shape: expected result rank >= source rank

// -----

// Invalid: reassociation group must be an array attribute.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [0 : i32, [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation group 0 must be an array attribute

// -----

// Invalid: reassociation indices must be i32 integer attributes.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i64], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation group 0 index 0 must be an i32 integer attribute

// -----

// Invalid: reassociation groups must be non-empty.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [], [1 : i32, 2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation groups must be non-empty

// -----

// Invalid: reassociation indices must be non-negative.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[-1 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation indices must be non-negative
