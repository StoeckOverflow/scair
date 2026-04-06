builtin.module {
  func.func private @control_flow_selected_allocation_reduction_impl(
    %sel : i1,
    %n : index
  ) -> i64 {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %i1 = "arith.constant"() <{value = 1 : i64}> : () -> i64
    %i2 = "arith.constant"() <{value = 2 : i64}> : () -> i64
    %i0 = "arith.constant"() <{value = 0 : i64}> : () -> i64
    %n_nat = "dtensor.index_to_nat"(%n) : (index) -> !dtensor.nat

    // Strongest currently executable refined route:
    // the benchmark core is expressed directly in d_memref/d_affine, so the
    // selected allocation lowers to pointer-based carrier state instead of a
    // descriptor struct.
    %buf = "scf.if"(%sel) ({
      %route0 = "d_memref.alloc"() : () -> !d_memref.memref<[%n_nat], i64>

      %carry0 = "d_affine.for"(%c0, %n, %i1) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 1>
      }> ({
      ^bb0(%i: index, %cur_value: i64):
        "d_memref.store"(%cur_value, %route0, %i) : (i64, !d_memref.memref<[%n_nat], i64>, index) -> ()
        %next_value = "arith.addi"(%cur_value, %i1) : (i64, i64) -> i64
        "d_affine.yield"(%next_value) : (i64) -> ()
      }) : (index, index, i64) -> i64

      "scf.yield"(%route0) : (!d_memref.memref<[%n_nat], i64>) -> ()
    }, {
      %route1 = "d_memref.alloc"() : () -> !d_memref.memref<[%n_nat], i64>

      %carry1 = "d_affine.for"(%c0, %n, %i2) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 1>
      }> ({
      ^bb0(%i: index, %cur_value: i64):
        "d_memref.store"(%cur_value, %route1, %i) : (i64, !d_memref.memref<[%n_nat], i64>, index) -> ()
        %next_value = "arith.addi"(%cur_value, %i2) : (i64, i64) -> i64
        "d_affine.yield"(%next_value) : (i64) -> ()
      }) : (index, index, i64) -> i64

      "scf.yield"(%route1) : (!d_memref.memref<[%n_nat], i64>) -> ()
    }) : (i1) -> !d_memref.memref<[%n_nat], i64>

    %carry_buf, %sum = "d_affine.for"(%c0, %n, %buf, %i0) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 2>
    }> ({
    ^bb0(%i: index, %cur: !d_memref.memref<[%n_nat], i64>, %acc: i64):
      %v = "d_memref.load"(%cur, %i) : (!d_memref.memref<[%n_nat], i64>, index) -> i64
      %next = "arith.addi"(%acc, %v) : (i64, i64) -> i64
      "d_affine.yield"(%cur, %next) : (!d_memref.memref<[%n_nat], i64>, i64) -> ()
    }) : (index, index, !d_memref.memref<[%n_nat], i64>, i64) -> (!d_memref.memref<[%n_nat], i64>, i64)

    "d_memref.dealloc"(%carry_buf) : (!d_memref.memref<[%n_nat], i64>) -> ()
    "func.return"(%sum) : (i64) -> ()
  }

  // The public wrapper keeps the same observable benchmark interface as the
  // baseline executable while the benchmark core above stays fully refined.
  func.func @control_flow_selected_allocation_reduction(
    %sel : i1,
    %n : index,
    %out : memref<1xi64>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %sum = "func.call"(%sel, %n) <{callee = @control_flow_selected_allocation_reduction_impl}> : (i1, index) -> i64
    "memref.store"(%sum, %out, %c0) : (i64, memref<1xi64>, index) -> ()
    "func.return"() : () -> ()
  }
}
