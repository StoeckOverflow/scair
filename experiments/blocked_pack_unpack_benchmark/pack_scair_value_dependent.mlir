builtin.module {
  func.func @blocked_pack(
    %mo_nat : !dtensor.nat,
    %no_nat : !dtensor.nat,
    %tm_nat : !dtensor.nat,
    %tn_nat : !dtensor.nat,
    %src_flat : !d_memref.memref<[], i64>,
    %dst_flat : !d_memref.memref<[], i64>
  ) attributes {scair.emit_bare_interface = true} {
    %m_nat = "dtensor.nat.mul"(%mo_nat, %tm_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %n_nat = "dtensor.nat.mul"(%no_nat, %tn_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index

    %mo = "dtensor.shape.to_index"(%mo_nat) : (!dtensor.nat) -> index
    %no = "dtensor.shape.to_index"(%no_nat) : (!dtensor.nat) -> index
    %tm = "dtensor.shape.to_index"(%tm_nat) : (!dtensor.nat) -> index
    %tn = "dtensor.shape.to_index"(%tn_nat) : (!dtensor.nat) -> index
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index

    %dst_stride_mi = "arith.muli"(%tn, %c1) : (index, index) -> index
    %dst_stride_no = "arith.muli"(%tm, %tn) : (index, index) -> index
    %dst_stride_mo = "arith.muli"(%no, %dst_stride_no) : (index, index) -> index

    %src = d_memref.reinterpret_cast %src_flat
      : !d_memref.memref<[], i64>
        to !d_memref.memref<[%m_nat, %n_nat], i64, offset: 0, strides: [%n, %c1]>

    %dst = d_memref.reinterpret_cast %dst_flat
      : !d_memref.memref<[], i64>
        to !d_memref.memref<[%mo_nat, %no_nat, %tm_nat, %tn_nat], i64,
             offset: 0, strides: [%dst_stride_mo, %dst_stride_no, %dst_stride_mi, %c1]>

    d_affine.for %mo_i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%mo) step 1 : index {
      %base_m = "arith.muli"(%mo_i, %tm) : (index, index) -> index
      d_affine.for %no_i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%no) step 1 : index {
        %base_n = "arith.muli"(%no_i, %tn) : (index, index) -> index
        d_affine.for %mi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%tm) step 1 : index {
          %row = "arith.addi"(%base_m, %mi) : (index, index) -> index
          d_affine.for %ni = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%tn) step 1 : index {
            %col = "arith.addi"(%base_n, %ni) : (index, index) -> index
            %value = d_memref.load %src[%row, %col] : !d_memref.memref<[%m_nat, %n_nat], i64, offset: 0, strides: [%n, %c1]> -> i64
            d_memref.store %value, %dst[%mo_i, %no_i, %mi, %ni] : i64, !d_memref.memref<[%mo_nat, %no_nat, %tm_nat, %tn_nat], i64, offset: 0, strides: [%dst_stride_mo, %dst_stride_no, %dst_stride_mi, %c1]>
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
