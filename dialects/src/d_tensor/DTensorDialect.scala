package scair.dialects.d_tensor

import scair.clair.*
import scair.ir.*

val DTensorDialect = summonDialect[
  (DTensorNatType, DTensorPosNatType, DTensorVectorType, DTensorMatrixType, DTensorTensorType),
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
