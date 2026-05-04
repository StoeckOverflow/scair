module {
  func.func @blocked_pack(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: memref<?xi64>, %arg5: memref<?xi64>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %0 = arith.muli %arg0, %arg2 : index
    %1 = arith.muli %arg1, %arg3 : index
    scf.for %arg6 = %c0 to %arg0 step %c1 {
      %2 = arith.muli %arg6, %arg2 : index
      %3 = arith.subi %0, %2 : index
      %4 = arith.minsi %arg2, %3 : index
      scf.for %arg7 = %c0 to %arg1 step %c1 {
        %5 = arith.muli %arg7, %arg3 : index
        %6 = arith.subi %1, %5 : index
        %7 = arith.minsi %arg3, %6 : index
        scf.for %arg8 = %c0 to %4 step %c1 {
          %8 = arith.addi %2, %arg8 : index
          scf.for %arg9 = %c0 to %7 step %c1 {
            %9 = arith.addi %5, %arg9 : index
            %10 = arith.muli %8, %1 : index
            %11 = arith.addi %10, %9 : index
            %12 = arith.muli %arg6, %arg1 : index
            %13 = arith.addi %12, %arg7 : index
            %14 = arith.muli %13, %arg2 : index
            %15 = arith.addi %14, %arg8 : index
            %16 = arith.muli %15, %arg3 : index
            %17 = arith.addi %16, %arg9 : index
            %18 = memref.load %arg4[%11] : memref<?xi64>
            memref.store %18, %arg5[%17] : memref<?xi64>
          }
        }
      }
    }
    return
  }
}

