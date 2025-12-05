package scair.dialects.dlam

import scair.ir.*
import scair.clair.macros.*

val DlamDialect = summonDialect[
  // Custom attributes
  (DlamTypeType, DlamBVarType, DlamForAllType, DlamFunType, DepType, DlamTVarType),
  // Operations
  (VLambda, VReturn, TLambda, TReturn, TApply, VApply)
]
