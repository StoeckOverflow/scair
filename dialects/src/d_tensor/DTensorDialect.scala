package scair.dialects.d_tensor

import scair.clair.*
import scair.ir.*

val DTensorDialect = summonDialect[
  (DTensorVectorType, DTensorMatrixType, DTensorTensorType),
  (
    AssumeExtent,
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
