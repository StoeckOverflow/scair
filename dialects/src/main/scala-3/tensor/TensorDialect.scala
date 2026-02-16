package scair.dialects.tensor

import scair.clair.macros.*
import scair.ir.*

val TensorDialect = summonDialect[
  (TensorNatType, TensorVectorType, TensorMatrixType, TensorTensorType),
  (NatConst, NatAdd, NatMul, Empty, Fill, Dim, Add, Mul, Matmul, Cast),
]
