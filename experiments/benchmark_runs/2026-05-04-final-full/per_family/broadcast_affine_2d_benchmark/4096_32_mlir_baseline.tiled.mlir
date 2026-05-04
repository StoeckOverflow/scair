module {
  func.func @broadcast_affine_2d(%arg0: index, %arg1: index, %arg2: memref<?xi64>, %arg3: memref<?xi64>, %arg4: memref<?xi64>, %arg5: memref<?xi64>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %0 = arith.muli %arg0, %arg1 : index
    scf.for %arg6 = %c0 to %0 step %arg1 {
      %1 = arith.subi %0, %arg6 : index
      %2 = arith.minsi %arg1, %1 : index
      scf.for %arg7 = %c0 to %2 step %c1 {
        %3 = arith.addi %arg6, %arg7 : index
        %4 = memref.load %arg2[%3] : memref<?xi64>
        %5 = memref.load %arg3[%arg7] : memref<?xi64>
        %6 = memref.load %arg4[%arg7] : memref<?xi64>
        %7 = arith.muli %4, %5 : i64
        %8 = arith.addi %7, %6 : i64
        memref.store %8, %arg5[%3] : memref<?xi64>
      }
    }
    return
  }
}

