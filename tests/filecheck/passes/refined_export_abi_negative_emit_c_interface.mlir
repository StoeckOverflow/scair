// RUN: not scair-opt %s --passes lower-dmemref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi 2>&1 | filecheck %s

builtin.module {
  func.func @bad_c_iface(
    %n_nat : !dtensor.nat,
    %buf : !d_memref.memref<[%n_nat], f32>
  ) attributes {llvm.emit_c_interface = true} {
    func.return
  }
}

// CHECK: original external ABI metadata for bad_c_iface was overwritten
