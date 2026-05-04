"builtin.module"() ({
  "func.func"() ({
  ^bb0(
      %mo : index,
      %no : index,
      %tm : index,
      %tn : index,
      %src : memref<?xi64>,
      %dst : memref<?xi64>):
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %m = arith.muli %mo, %tm : index
    %n = arith.muli %no, %tn : index

    scf.for %mo_i = %c0 to %mo step %c1 {
      %base_m = arith.muli %mo_i, %tm : index
      %m_remaining = arith.subi %m, %base_m : index
      %mi_ub = arith.minsi %tm, %m_remaining : index
      scf.for %no_i = %c0 to %no step %c1 {
        %base_n = arith.muli %no_i, %tn : index
        %n_remaining = arith.subi %n, %base_n : index
        %ni_ub = arith.minsi %tn, %n_remaining : index
        scf.for %mi = %c0 to %mi_ub step %c1 {
          %row = arith.addi %base_m, %mi : index
          scf.for %ni = %c0 to %ni_ub step %c1 {
            %col = arith.addi %base_n, %ni : index

            %src_row = arith.muli %row, %n : index
            %src_idx = arith.addi %src_row, %col : index

            %dst_mo = arith.muli %mo_i, %no : index
            %dst_mono = arith.addi %dst_mo, %no_i : index
            %dst_block = arith.muli %dst_mono, %tm : index
            %dst_with_mi = arith.addi %dst_block, %mi : index
            %dst_inner = arith.muli %dst_with_mi, %tn : index
            %dst_idx = arith.addi %dst_inner, %ni : index

            %value = memref.load %src[%src_idx] : memref<?xi64>
            memref.store %value, %dst[%dst_idx] : memref<?xi64>
          }
        }
      }
    }
    "func.return"() : () -> ()
  }) {
    function_type = (index, index, index, index, memref<?xi64>, memref<?xi64>) -> (),
    llvm.emit_c_interface,
    sym_name = "blocked_pack"
  } : () -> ()
}) : () -> ()
