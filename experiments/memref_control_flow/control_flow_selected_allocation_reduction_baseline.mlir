builtin.module {
  func.func @control_flow_selected_allocation_reduction(
    %sel : i1,
    %n : index,
    %out : memref<1xi64>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %i1 = "arith.constant"() <{value = 1 : i64}> : () -> i64
    %i2 = "arith.constant"() <{value = 2 : i64}> : () -> i64
    %i0 = "arith.constant"() <{value = 0 : i64}> : () -> i64

    %buf = "scf.if"(%sel) ({
      %route0 = "memref.alloc"(%n)
        <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}>
        : (index) -> memref<?xi64>

      %carry0 = affine.for %i = affine_map<() -> (0)>() to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%cur_value = %i1 : i64) {
        "memref.store"(%cur_value, %route0, %i) : (i64, memref<?xi64>, index) -> ()
        %next_value = "arith.addi"(%cur_value, %i1) : (i64, i64) -> i64
        "affine.yield"(%next_value) : (i64) -> ()
      }

      "scf.yield"(%route0) : (memref<?xi64>) -> ()
    }, {
      %route1 = "memref.alloc"(%n)
        <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}>
        : (index) -> memref<?xi64>

      %carry1 = affine.for %i = affine_map<() -> (0)>() to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%cur_value = %i2 : i64) {
        "memref.store"(%cur_value, %route1, %i) : (i64, memref<?xi64>, index) -> ()
        %next_value = "arith.addi"(%cur_value, %i2) : (i64, i64) -> i64
        "affine.yield"(%next_value) : (i64) -> ()
      }

      "scf.yield"(%route1) : (memref<?xi64>) -> ()
    }) : (i1) -> memref<?xi64>

    %sum = affine.for %i = affine_map<() -> (0)>() to affine_map<(d0) -> (d0)>(%n) step 1 : index
        iter_args(%acc = %i0 : i64) {
      %v = "memref.load"(%buf, %i) : (memref<?xi64>, index) -> i64
      %next = "arith.addi"(%acc, %v) : (i64, i64) -> i64
      "affine.yield"(%next) : (i64) -> ()
    }

    "memref.store"(%sum, %out, %c0) : (i64, memref<1xi64>, index) -> ()
    "memref.dealloc"(%buf) : (memref<?xi64>) -> ()
    "func.return"() : () -> ()
  }
}
