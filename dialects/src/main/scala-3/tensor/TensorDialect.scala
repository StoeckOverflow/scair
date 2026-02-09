package scair.dialects.tensor

import scair.clair.macros.*
import scair.ir.*

val TensorDialect = summonDialect[
  (TensorVectorType, TensorMatrixType, TensorTensorType),
  (VAdd, MAdd, TAdd, VMul, MMul, TMul),
]
