package scair.dialects.dTensor

import scair.clair.*
import scair.ir.*

val dTensorDialect = summonDialect[
  (dTensorNatType, dTensorVectorType, dTensorMatrixType, dTensorTensorType),
  (
    NatConst,
    NatParam,
    NatAdd,
    NatMul,
    ShapeToIndex,
    IndexToNat,
    Empty,
    Fill,
    Dim,
    Add,
    Mul,
    Matmul,
    Cast,
    ExpandShape,
  ),
]
