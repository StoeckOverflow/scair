builtin.module {
  func.func private @control_flow_selected_allocation_reduction_impl(
    %sel : i1,
    %n_nat : !dtensor.nat
  ) -> i64 {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %i1 = "arith.constant"() <{value = 1 : i64}> : () -> i64
    %i2 = "arith.constant"() <{value = 2 : i64}> : () -> i64
    %i0 = "arith.constant"() <{value = 0 : i64}> : () -> i64
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index

    %buf = "scf.if"(%sel) ({
      %route0 = "d_memref.alloc"() : () -> !d_memref.memref<[%n_nat], i64>

      %carry0 = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%cur_value = %i1 : i64) {
        "d_memref.store"(%cur_value, %route0, %i) : (i64, !d_memref.memref<[%n_nat], i64>, index) -> ()
        %next_value = "arith.addi"(%cur_value, %i1) : (i64, i64) -> i64
        d_affine.yield %next_value : (i64)
      }

      "scf.yield"(%route0) : (!d_memref.memref<[%n_nat], i64>) -> ()
    }, {
      %route1 = "d_memref.alloc"() : () -> !d_memref.memref<[%n_nat], i64>

      %carry1 = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%cur_value = %i2 : i64) {
        "d_memref.store"(%cur_value, %route1, %i) : (i64, !d_memref.memref<[%n_nat], i64>, index) -> ()
        %next_value = "arith.addi"(%cur_value, %i2) : (i64, i64) -> i64
        d_affine.yield %next_value : (i64)
      }

      "scf.yield"(%route1) : (!d_memref.memref<[%n_nat], i64>) -> ()
    }) : (i1) -> !d_memref.memref<[%n_nat], i64>

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc = %i0 : i64) {
      %v = "d_memref.load"(%buf, %i) : (!d_memref.memref<[%n_nat], i64>, index) -> i64
      %next = "arith.addi"(%acc, %v) : (i64, i64) -> i64
      d_affine.yield %next : (i64)
    }

    "d_memref.dealloc"(%buf) : (!d_memref.memref<[%n_nat], i64>) -> ()
    "func.return"(%sum) : (i64) -> ()
  }

  func.func @control_flow_selected_allocation_reduction(
    %sel : i1,
    %n_nat : !dtensor.nat,
    %out_nat : !dtensor.nat,
    %out : !d_memref.memref<[%out_nat], i64>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %sum = "func.call"(%sel, %n_nat) <{callee = @control_flow_selected_allocation_reduction_impl}> : (i1, !dtensor.nat) -> i64
    d_memref.store %sum, %out[%c0] : i64, !d_memref.memref<[%out_nat], i64>
    "func.return"() : () -> ()
  }
}
