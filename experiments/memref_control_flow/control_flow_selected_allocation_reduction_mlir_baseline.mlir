builtin.module {
  func.func @control_flow_selected_allocation_reduction(
    %sel : i1,
    %n : index,
    %out : memref<1xi64>
  ) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %i1 = arith.constant 1 : i64
    %i2 = arith.constant 2 : i64
    %i0 = arith.constant 0 : i64

    %buf = scf.if %sel -> (memref<?xi64>) {
      %route0 = memref.alloc(%n) : memref<?xi64>
      %carry0 = affine.for %i = 0 to %n iter_args(%cur_value = %i1) -> (i64) {
        memref.store %cur_value, %route0[%i] : memref<?xi64>
        %next_value = arith.addi %cur_value, %i1 : i64
        affine.yield %next_value : i64
      }
      scf.yield %route0 : memref<?xi64>
    } else {
      %route1 = memref.alloc(%n) : memref<?xi64>
      %carry1 = affine.for %i = 0 to %n iter_args(%cur_value = %i2) -> (i64) {
        memref.store %cur_value, %route1[%i] : memref<?xi64>
        %next_value = arith.addi %cur_value, %i2 : i64
        affine.yield %next_value : i64
      }
      scf.yield %route1 : memref<?xi64>
    }

    %carry_buf, %sum = affine.for %i = 0 to %n
        iter_args(%cur = %buf, %acc = %i0)
        -> (memref<?xi64>, i64) {
      %v = memref.load %cur[%i] : memref<?xi64>
      %next = arith.addi %acc, %v : i64
      affine.yield %cur, %next : memref<?xi64>, i64
    }

    memref.store %sum, %out[%c0] : memref<1xi64>
    memref.dealloc %carry_buf : memref<?xi64>
    return
  }
}
