package scair.dialects.dTensor

import scair.clair.macros.*
import scair.ir.*

val dTensorDialect = summonDialect[
  (dTensorNatType, dTensorVectorType, dTensorMatrixType, dTensorTensorType),
  (NatConst, NatAdd, NatMul, Empty, Fill, Dim, Add, Mul, Matmul, Cast),
]
