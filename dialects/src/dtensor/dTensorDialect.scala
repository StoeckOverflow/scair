package scair.dialects.dTensor

import scair.clair.*
import scair.ir.*

val dTensorDialect = summonDialect[
  (dTensorNatType, dTensorPosNatType, dTensorVectorType, dTensorMatrixType, dTensorTensorType),
  (
    NatConst,
    NatParam,
    NatAdd,
    NatMul,
    ShapeToIndex,
    IndexToNat,
    NatRefinePositive,
    Empty,
    Fill,
    Dim,
    Add,
    Mul,
    Matmul,
    Cast,
    ExpandShape,
    CollapseShape,
    SplitDim,
    JoinDim,
    PermuteDims,
  ),
]
