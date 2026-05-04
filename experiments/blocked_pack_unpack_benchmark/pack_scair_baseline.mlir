builtin.module {
  func.func @blocked_pack(
    %mo : index,
    %no : index,
    %tm : index,
    %tn : index,
    %src_flat : memref<?xi64>,
    %dst_flat : memref<?xi64>
  ) attributes {scair.emit_descriptor_pointer_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %m = "arith.muli"(%mo, %tm) : (index, index) -> index
    %n = "arith.muli"(%no, %tn) : (index, index) -> index
    %src_stride_mo = "arith.muli"(%tm, %n) : (index, index) -> index
    %dst_stride_mi = "arith.muli"(%tn, %c1) : (index, index) -> index
    %dst_stride_no = "arith.muli"(%tm, %tn) : (index, index) -> index
    %dst_stride_mo = "arith.muli"(%no, %dst_stride_no) : (index, index) -> index

    %src = "memref.reinterpret_cast"(%src_flat, %c0, %mo, %no, %tm, %tn, %src_stride_mo, %tn, %n, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xi64>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>

    %dst = "memref.reinterpret_cast"(%dst_flat, %c0, %mo, %no, %tm, %tn, %dst_stride_mo, %dst_stride_no, %dst_stride_mi, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xi64>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>

    affine.for %mo_i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%mo) step 1 : index {
      affine.for %no_i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%no) step 1 : index {
        affine.for %mi = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%tm) step 1 : index {
          affine.for %ni = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%tn) step 1 : index {
            %value = "memref.load"(%src, %mo_i, %no_i, %mi, %ni) : (memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> i64
            "memref.store"(%value, %dst, %mo_i, %no_i, %mi, %ni) : (i64, memref<?x?x?x?xi64, strided<[?, ?, ?, ?], offset: 0>>, index, index, index, index) -> ()
          }
        }
      }
    }
    "func.return"() : () -> ()
  }
}
