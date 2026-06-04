package scair.dialects

import scair.dialects.affine.AffineDialect
import scair.dialects.arith.ArithDialect
import scair.dialects.builtin.BuiltinDialect
import scair.dialects.cf.CFDialect
import scair.dialects.cmath.cmath
import scair.dialects.complex.Complex
import scair.dialects.d_affine.dAffineDialect
import scair.dialects.d_memref.dMemrefDialect
import scair.dialects.func.FuncDialect
import scair.dialects.irdl.IRDL
import scair.dialects.lingodb.*
import scair.dialects.llvm.LLVMDialect
import scair.dialects.math.MathDialect
import scair.dialects.memref.MemrefDialect
import scair.dialects.scf.SCFDialect
import scair.dialects.dTensor.dTensorDialect
import scair.dialects.test.Test
import scair.dialects.tlam_de_bruijn.TlamDeBruijnDialect
import scair.ir.Dialect
import scair.dialects.tlam.TlamDialect

//
// ░█████╗░ ██╗░░░░░ ██╗░░░░░
// ██╔══██╗ ██║░░░░░ ██║░░░░░
// ███████║ ██║░░░░░ ██║░░░░░
// ██╔══██║ ██║░░░░░ ██║░░░░░
// ██║░░██║ ███████╗ ███████╗
// ╚═╝░░╚═╝ ╚══════╝ ╚══════╝
//
// ██████╗░ ██╗ ░█████╗░ ██╗░░░░░ ███████╗ ░█████╗░ ████████╗ ░██████╗
// ██╔══██╗ ██║ ██╔══██╗ ██║░░░░░ ██╔════╝ ██╔══██╗ ╚══██╔══╝ ██╔════╝
// ██║░░██║ ██║ ███████║ ██║░░░░░ █████╗░░ ██║░░╚═╝ ░░░██║░░░ ╚█████╗░
// ██║░░██║ ██║ ██╔══██║ ██║░░░░░ ██╔══╝░░ ██║░░██╗ ░░░██║░░░ ░╚═══██╗
// ██████╔╝ ██║ ██║░░██║ ███████╗ ███████╗ ╚█████╔╝ ░░░██║░░░ ██████╔╝
// ╚═════╝░ ╚═╝ ╚═╝░░╚═╝ ╚══════╝ ╚══════╝ ░╚════╝░ ░░░╚═╝░░░ ╚═════╝░
//

val allDialects: Seq[Dialect] =
  Seq(
    BuiltinDialect,
    Complex,
    dTensorDialect,
    MathDialect,
    TlamDeBruijnDialect,
    Test,
    IRDL,
    ArithDialect,
    CFDialect,
    MemrefDialect,
    dMemrefDialect,
    cmath,
    AffineDialect,
    dAffineDialect,
    FuncDialect,
    LLVMDialect,
    SCFDialect,
    TlamDialect,
    DbDialect,
    TuplesDialect,
    RelAlgDialect,
    SubopDialect,
  )
