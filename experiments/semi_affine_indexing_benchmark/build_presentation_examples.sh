#!/usr/bin/env bash
set -euo pipefail

EXAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="${OUT_DIR:-$EXAMPLE_DIR/build_presentation}"
GENERATED_DIR="$OUT_DIR/generated"
mkdir -p "$GENERATED_DIR"

MLIR_PRESENTATION_SRC="$GENERATED_DIR/semi_affine_presentation_mlir_baseline.mlir"
VALUE_DEP_PRESENTATION_SRC="$GENERATED_DIR/version2.mlir"

cat > "$MLIR_PRESENTATION_SRC" <<'MLIR'
builtin.module {
  func.func @semi_affine_fill_and_sum(
      %stride0 : index,
      %stride1 : index,
      %flat : memref<?xf32>,
      %out : memref<1xf32>) attributes {llvm.emit_c_interface} {
    %c256 = arith.constant 256 : index
    %c1024 = arith.constant 1024 : index
    %c0 = arith.constant 0 : index
    %f0 = arith.constant 0.0 : f32
    %f1 = arith.constant 1.0 : f32

    %buf = memref.reinterpret_cast %flat to
      offset: [0],
      sizes: [256, 1024],
      strides: [%stride0, %stride1]
    : memref<?xf32> to memref<256x1024xf32, strided<[?, ?], offset: 0>>

    affine.for %i = 0 to 256 {
      affine.for %j = 0 to 1024 {
        affine.store %f1, %buf[%i, %j] : memref<256x1024xf32, strided<[?, ?], offset: 0>>
      }
    }

    %sum = affine.for %i = 0 to 256 iter_args(%acc = %f0) -> (f32) {
      %inner = affine.for %j = 0 to 1024 iter_args(%acc2 = %acc) -> (f32) {
        %v = affine.load %buf[%i, %j] : memref<256x1024xf32, strided<[?, ?], offset: 0>>
        %next = arith.addf %acc2, %v : f32
        affine.yield %next : f32
      }
      affine.yield %inner : f32
    }

    memref.store %sum, %out[%c0] : memref<1xf32>
    return
  }
}
MLIR

cat > "$VALUE_DEP_PRESENTATION_SRC" <<'MLIR'
builtin.module {
  func.func @semi_affine_fill_and_sum(
    %stride0 : index,
    %stride1 : index,
    %buf : !d_memref.memref<[256, 1024], f32, offset: 0, strides: [%stride0, %stride1]>,
    %out : !d_memref.memref<[1], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c256) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index {
        d_memref.store %f1, %buf[%i, %j] : f32, !d_memref.memref<[256, 1024], f32, offset: 0, strides: [%stride0, %stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c256) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[256, 1024], f32, offset: 0, strides: [%stride0, %stride1]> -> f32
        %next = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %next : (f32)
      }
      d_affine.yield %inner : (f32)
    }

    d_memref.store %sum, %out[%c0] : f32, !d_memref.memref<[1], f32>
    "func.return"() : () -> ()
  }
}
MLIR

OUT_DIR="$OUT_DIR" \
MLIR_BASELINE_SRC="$MLIR_PRESENTATION_SRC" \
BASELINE_SRC="$EXAMPLE_DIR/semi_affine_kernel_scair_baseline.mlir" \
VALUE_DEP_SRC="$VALUE_DEP_PRESENTATION_SRC" \
bash "$EXAMPLE_DIR/build_scair_example.sh"

echo
echo "Presentation examples generated in $OUT_DIR"
echo "Runnable presentation sources:"
echo "  $MLIR_PRESENTATION_SRC"
echo "  $VALUE_DEP_PRESENTATION_SRC"
echo "LLVM dialect MLIR:"
echo "  $OUT_DIR/semi_affine_mlir_baseline.llvm.mlir"
echo "  $OUT_DIR/semi_affine_value_dependent_scair.llvm.mlir"
echo "LLVM IR:"
echo "  $OUT_DIR/semi_affine_mlir_baseline.ll"
echo "  $OUT_DIR/semi_affine_value_dependent_scair.ll"
