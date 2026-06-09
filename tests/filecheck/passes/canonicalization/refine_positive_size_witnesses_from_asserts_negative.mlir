// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-size-witnesses-from-asserts | filecheck %s

builtin.module {
  func.func @non_strict_nonnegative_is_not_positive(%k_idx: index) {
    %k = "d_tensor.size.import"(%k_idx) : (index) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k_idx, %c0) <{predicate = 5 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k may be zero"}> : (i1) -> ()
    "test.keep"(%k) : (!d_tensor.size) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @non_strict_nonnegative_is_not_positive
// CHECK-NOT: d_tensor.size.refine_positive
