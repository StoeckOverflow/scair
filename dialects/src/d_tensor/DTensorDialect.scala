package scair.dialects.d_tensor

import scair.clair.*
import scair.ir.*

val DTensorDialect = summonDialect[
  (DTensorSizeType, DTensorPosSizeType, DTensorPositiveSizeProofType, DTensorVectorType, DTensorMatrixType, DTensorTensorType),
  (
    SizeConstant,
    SizeParam,
    SizeAdd,
    SizeMul,
    SizeImport,
    SizePositiveProof,
    SizeRefinePositive,
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
