builtin.module {
  func.func @broadcast_affine_2d(
      %k0 : index,
      %k1 : index,
      %X : memref<?xi64>,
      %scale : memref<?xi64>,
      %bias : memref<?xi64>,
      %Y : memref<?xi64>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %k = arith.muli %k0, %k1 : index

    scf.for %base = %c0 to %k step %k1 {
      %remaining = arith.subi %k, %base : index
      %ub = arith.minsi %k1, %remaining : index
      scf.for %j = %c0 to %ub step %c1 {
        %i = arith.addi %base, %j : index
        %x = memref.load %X[%i] : memref<?xi64>
        %s = memref.load %scale[%j] : memref<?xi64>
        %b = memref.load %bias[%j] : memref<?xi64>
        %mul = arith.muli %x, %s : i64
        %out = arith.addi %mul, %b : i64
        memref.store %out, %Y[%i] : memref<?xi64>
      }
    }
    "func.return"() : () -> ()
  }
}
