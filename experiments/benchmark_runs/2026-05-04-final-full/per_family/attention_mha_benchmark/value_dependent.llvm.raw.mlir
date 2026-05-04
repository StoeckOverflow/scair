builtin.module {
  llvm.func @bench_expf(f32) -> f32
  llvm.func @bench_inv_sqrt_index(i64) -> f32
  llvm.func @attention_mha(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr, %6: !llvm.ptr, %7: !llvm.ptr, %8: !llvm.ptr, %9: !llvm.ptr, %10: !llvm.ptr) {
%11 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%12 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%13 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%14 = "llvm.mlir.constant"() <{value = -3.40282347E38 : f32}> : () -> f32
%15 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%16 = "llvm.mul"(%1, %15) : (i64, i64) -> i64
%17 = "llvm.mul"(%1, %1) : (i64, i64) -> i64
%18 = "llvm.mul"(%2, %17) : (i64, i64) -> i64
%19 = llvm.call @bench_inv_sqrt_index(%3) : (i64) -> f32
"llvm.br"(%12)[^bb0] : (i64) -> ()
  ^bb0(%20: i64):
    %21 = "llvm.icmp"(%20, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%21)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%12)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.br"(%12)[^bb4] : (i64) -> ()
  ^bb3(%22: i64):
    %23 = "llvm.icmp"(%22, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%23)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb5:
    %24 = "llvm.mul"(%22, %3) : (i64, i64) -> i64
    "llvm.br"(%12)[^bb7] : (i64) -> ()
  ^bb6:
    %25 = "llvm.add"(%20, %11) : (i64, i64) -> i64
    "llvm.br"(%25)[^bb0] : (i64) -> ()
  ^bb7(%26: i64):
    %27 = "llvm.icmp"(%26, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%27)[^bb8, ^bb9] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb8:
    "llvm.br"(%12)[^bb10] : (i64) -> ()
  ^bb9:
    %28 = "llvm.add"(%22, %11) : (i64, i64) -> i64
    "llvm.br"(%28)[^bb3] : (i64) -> ()
  ^bb10(%29: i64):
    %30 = "llvm.icmp"(%29, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%30)[^bb11, ^bb12] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb11:
    "llvm.br"(%12, %13)[^bb13] : (i64, f32) -> ()
  ^bb12:
    %31 = "llvm.add"(%26, %11) : (i64, i64) -> i64
    "llvm.br"(%31)[^bb7] : (i64) -> ()
  ^bb13(%32: i64, %33: f32):
    %34 = "llvm.icmp"(%32, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%34)[^bb14, ^bb15] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb14:
    %35 = "llvm.add"(%24, %32) : (i64, i64) -> i64
    %36 = "llvm.mul"(%20, %16) : (i64, i64) -> i64
    %37 = "llvm.mul"(%26, %15) : (i64, i64) -> i64
    %38 = "llvm.add"(%36, %37) : (i64, i64) -> i64
    %39 = "llvm.add"(%38, %35) : (i64, i64) -> i64
    %40 = "llvm.getelementptr"(%4, %39) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %41 = llvm.load %40 : !llvm.ptr -> f32
    %42 = "llvm.mul"(%20, %16) : (i64, i64) -> i64
    %43 = "llvm.mul"(%29, %15) : (i64, i64) -> i64
    %44 = "llvm.add"(%42, %43) : (i64, i64) -> i64
    %45 = "llvm.add"(%44, %35) : (i64, i64) -> i64
    %46 = "llvm.getelementptr"(%5, %45) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %47 = llvm.load %46 : !llvm.ptr -> f32
    %48 = "llvm.fmul"(%41, %47) : (f32, f32) -> f32
    %49 = "llvm.fadd"(%33, %48) : (f32, f32) -> f32
    %50 = "llvm.add"(%32, %11) : (i64, i64) -> i64
    "llvm.br"(%50, %49)[^bb13] : (i64, f32) -> ()
  ^bb15:
    %51 = "llvm.fmul"(%33, %19) : (f32, f32) -> f32
    %52 = "llvm.mul"(%20, %18) : (i64, i64) -> i64
    %53 = "llvm.mul"(%22, %17) : (i64, i64) -> i64
    %54 = "llvm.mul"(%26, %1) : (i64, i64) -> i64
    %55 = "llvm.add"(%52, %53) : (i64, i64) -> i64
    %56 = "llvm.add"(%55, %54) : (i64, i64) -> i64
    %57 = "llvm.add"(%56, %29) : (i64, i64) -> i64
    %58 = "llvm.getelementptr"(%7, %57) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%51, %58) : (f32, !llvm.ptr) -> ()
    %59 = "llvm.add"(%29, %11) : (i64, i64) -> i64
    "llvm.br"(%59)[^bb10] : (i64) -> ()
  ^bb4(%60: i64):
    %61 = "llvm.icmp"(%60, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%61)[^bb16, ^bb17] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb16:
    "llvm.br"(%12)[^bb18] : (i64) -> ()
  ^bb17:
    "llvm.br"(%12)[^bb19] : (i64) -> ()
  ^bb18(%62: i64):
    %63 = "llvm.icmp"(%62, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%63)[^bb20, ^bb21] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb20:
    "llvm.br"(%12)[^bb22] : (i64) -> ()
  ^bb21:
    %64 = "llvm.add"(%60, %11) : (i64, i64) -> i64
    "llvm.br"(%64)[^bb4] : (i64) -> ()
  ^bb22(%65: i64):
    %66 = "llvm.icmp"(%65, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%66)[^bb23, ^bb24] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb23:
    "llvm.br"(%12, %14)[^bb25] : (i64, f32) -> ()
  ^bb24:
    %67 = "llvm.add"(%62, %11) : (i64, i64) -> i64
    "llvm.br"(%67)[^bb18] : (i64) -> ()
  ^bb25(%68: i64, %69: f32):
    %70 = "llvm.icmp"(%68, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%70)[^bb26, ^bb27] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb26:
    %71 = "llvm.mul"(%60, %18) : (i64, i64) -> i64
    %72 = "llvm.mul"(%62, %17) : (i64, i64) -> i64
    %73 = "llvm.mul"(%65, %1) : (i64, i64) -> i64
    %74 = "llvm.add"(%71, %72) : (i64, i64) -> i64
    %75 = "llvm.add"(%74, %73) : (i64, i64) -> i64
    %76 = "llvm.add"(%75, %68) : (i64, i64) -> i64
    %77 = "llvm.getelementptr"(%7, %76) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %78 = llvm.load %77 : !llvm.ptr -> f32
    %79 = "arith.maximumf"(%69, %78) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %80 = "llvm.add"(%68, %11) : (i64, i64) -> i64
    "llvm.br"(%80, %79)[^bb25] : (i64, f32) -> ()
  ^bb27:
    "llvm.br"(%12, %13)[^bb28] : (i64, f32) -> ()
  ^bb28(%81: i64, %82: f32):
    %83 = "llvm.icmp"(%81, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%83)[^bb29, ^bb30] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb29:
    %84 = "llvm.mul"(%60, %18) : (i64, i64) -> i64
    %85 = "llvm.mul"(%62, %17) : (i64, i64) -> i64
    %86 = "llvm.mul"(%65, %1) : (i64, i64) -> i64
    %87 = "llvm.add"(%84, %85) : (i64, i64) -> i64
    %88 = "llvm.add"(%87, %86) : (i64, i64) -> i64
    %89 = "llvm.add"(%88, %81) : (i64, i64) -> i64
    %90 = "llvm.getelementptr"(%7, %89) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %91 = llvm.load %90 : !llvm.ptr -> f32
    %92 = "arith.subf"(%91, %69) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %93 = llvm.call @bench_expf(%92) : (f32) -> f32
    %94 = "llvm.mul"(%60, %18) : (i64, i64) -> i64
    %95 = "llvm.mul"(%62, %17) : (i64, i64) -> i64
    %96 = "llvm.mul"(%65, %1) : (i64, i64) -> i64
    %97 = "llvm.add"(%94, %95) : (i64, i64) -> i64
    %98 = "llvm.add"(%97, %96) : (i64, i64) -> i64
    %99 = "llvm.add"(%98, %81) : (i64, i64) -> i64
    %100 = "llvm.getelementptr"(%8, %99) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%93, %100) : (f32, !llvm.ptr) -> ()
    %101 = "llvm.fadd"(%82, %93) : (f32, f32) -> f32
    %102 = "llvm.add"(%81, %11) : (i64, i64) -> i64
    "llvm.br"(%102, %101)[^bb28] : (i64, f32) -> ()
  ^bb30:
    "llvm.br"(%12)[^bb31] : (i64) -> ()
  ^bb31(%103: i64):
    %104 = "llvm.icmp"(%103, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%104)[^bb32, ^bb33] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb32:
    %105 = "llvm.mul"(%60, %18) : (i64, i64) -> i64
    %106 = "llvm.mul"(%62, %17) : (i64, i64) -> i64
    %107 = "llvm.mul"(%65, %1) : (i64, i64) -> i64
    %108 = "llvm.add"(%105, %106) : (i64, i64) -> i64
    %109 = "llvm.add"(%108, %107) : (i64, i64) -> i64
    %110 = "llvm.add"(%109, %103) : (i64, i64) -> i64
    %111 = "llvm.getelementptr"(%8, %110) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %112 = llvm.load %111 : !llvm.ptr -> f32
    %113 = "arith.divf"(%112, %82) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %114 = "llvm.mul"(%60, %18) : (i64, i64) -> i64
    %115 = "llvm.mul"(%62, %17) : (i64, i64) -> i64
    %116 = "llvm.mul"(%65, %1) : (i64, i64) -> i64
    %117 = "llvm.add"(%114, %115) : (i64, i64) -> i64
    %118 = "llvm.add"(%117, %116) : (i64, i64) -> i64
    %119 = "llvm.add"(%118, %103) : (i64, i64) -> i64
    %120 = "llvm.getelementptr"(%8, %119) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%113, %120) : (f32, !llvm.ptr) -> ()
    %121 = "llvm.add"(%103, %11) : (i64, i64) -> i64
    "llvm.br"(%121)[^bb31] : (i64) -> ()
  ^bb33:
    %122 = "llvm.add"(%65, %11) : (i64, i64) -> i64
    "llvm.br"(%122)[^bb22] : (i64) -> ()
  ^bb19(%123: i64):
    %124 = "llvm.icmp"(%123, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%124)[^bb34, ^bb35] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb34:
    "llvm.br"(%12)[^bb36] : (i64) -> ()
  ^bb35:
    "llvm.return"() : () -> ()
  ^bb36(%125: i64):
    %126 = "llvm.icmp"(%125, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%126)[^bb37, ^bb38] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb37:
    "llvm.br"(%12)[^bb39] : (i64) -> ()
  ^bb38:
    %127 = "llvm.add"(%123, %11) : (i64, i64) -> i64
    "llvm.br"(%127)[^bb19] : (i64) -> ()
  ^bb39(%128: i64):
    %129 = "llvm.icmp"(%128, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%129)[^bb40, ^bb41] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb40:
    %130 = "llvm.mul"(%128, %3) : (i64, i64) -> i64
    "llvm.br"(%12)[^bb42] : (i64) -> ()
  ^bb41:
    "llvm.br"(%12, %13)[^bb43] : (i64, f32) -> ()
  ^bb42(%131: i64):
    %132 = "llvm.icmp"(%131, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%132)[^bb44, ^bb45] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb44:
    %133 = "llvm.add"(%130, %131) : (i64, i64) -> i64
    "llvm.br"(%12, %13)[^bb46] : (i64, f32) -> ()
  ^bb45:
    %134 = "llvm.add"(%128, %11) : (i64, i64) -> i64
    "llvm.br"(%134)[^bb39] : (i64) -> ()
  ^bb46(%135: i64, %136: f32):
    %137 = "llvm.icmp"(%135, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%137)[^bb47, ^bb48] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb47:
    %138 = "llvm.mul"(%123, %18) : (i64, i64) -> i64
    %139 = "llvm.mul"(%128, %17) : (i64, i64) -> i64
    %140 = "llvm.mul"(%125, %1) : (i64, i64) -> i64
    %141 = "llvm.add"(%138, %139) : (i64, i64) -> i64
    %142 = "llvm.add"(%141, %140) : (i64, i64) -> i64
    %143 = "llvm.add"(%142, %135) : (i64, i64) -> i64
    %144 = "llvm.getelementptr"(%8, %143) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %145 = llvm.load %144 : !llvm.ptr -> f32
    %146 = "llvm.mul"(%123, %16) : (i64, i64) -> i64
    %147 = "llvm.mul"(%135, %15) : (i64, i64) -> i64
    %148 = "llvm.add"(%146, %147) : (i64, i64) -> i64
    %149 = "llvm.add"(%148, %133) : (i64, i64) -> i64
    %150 = "llvm.getelementptr"(%6, %149) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %151 = llvm.load %150 : !llvm.ptr -> f32
    %152 = "llvm.fmul"(%145, %151) : (f32, f32) -> f32
    %153 = "llvm.fadd"(%136, %152) : (f32, f32) -> f32
    %154 = "llvm.add"(%135, %11) : (i64, i64) -> i64
    "llvm.br"(%154, %153)[^bb46] : (i64, f32) -> ()
  ^bb48:
    %155 = "llvm.mul"(%123, %16) : (i64, i64) -> i64
    %156 = "llvm.mul"(%125, %15) : (i64, i64) -> i64
    %157 = "llvm.add"(%155, %156) : (i64, i64) -> i64
    %158 = "llvm.add"(%157, %133) : (i64, i64) -> i64
    %159 = "llvm.getelementptr"(%9, %158) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%136, %159) : (f32, !llvm.ptr) -> ()
    %160 = "llvm.add"(%131, %11) : (i64, i64) -> i64
    "llvm.br"(%160)[^bb42] : (i64) -> ()
  ^bb43(%161: i64, %162: f32):
    %163 = "llvm.icmp"(%161, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%163)[^bb49, ^bb50] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb49:
    "llvm.br"(%161, %12, %162)[^bb51] : (i64, i64, f32) -> ()
  ^bb51(%164: i64, %165: i64, %166: f32):
    %167 = "llvm.icmp"(%165, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%167)[^bb52, ^bb53] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb52:
    %168 = "llvm.mul"(%164, %3) : (i64, i64) -> i64
    %169 = "llvm.add"(%168, %165) : (i64, i64) -> i64
    %170 = "llvm.mul"(%123, %16) : (i64, i64) -> i64
    %171 = "llvm.mul"(%125, %15) : (i64, i64) -> i64
    %172 = "llvm.add"(%170, %171) : (i64, i64) -> i64
    %173 = "llvm.add"(%172, %169) : (i64, i64) -> i64
    %174 = "llvm.getelementptr"(%9, %173) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %175 = llvm.load %174 : !llvm.ptr -> f32
    %176 = "llvm.mul"(%123, %16) : (i64, i64) -> i64
    %177 = "llvm.mul"(%125, %15) : (i64, i64) -> i64
    %178 = "llvm.add"(%176, %177) : (i64, i64) -> i64
    %179 = "llvm.add"(%178, %169) : (i64, i64) -> i64
    %180 = "llvm.getelementptr"(%10, %179) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%175, %180) : (f32, !llvm.ptr) -> ()
    %181 = "llvm.add"(%165, %11) : (i64, i64) -> i64
    "llvm.br"(%164, %181, %175)[^bb51] : (i64, i64, f32) -> ()
  ^bb53:
    %182 = "llvm.add"(%164, %11) : (i64, i64) -> i64
    "llvm.br"(%182, %166)[^bb43] : (i64, f32) -> ()
  ^bb50:
    %183 = "llvm.add"(%125, %11) : (i64, i64) -> i64
    "llvm.br"(%183)[^bb36] : (i64) -> ()
  }
}
