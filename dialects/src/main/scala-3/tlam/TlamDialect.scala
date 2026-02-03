package scair.dialects.tlam

import scair.ir.*
import scair.clair.macros.*

val TlamDialect = summonDialect[
  // Custom attributes
  (TlamTypeType, TlamBVarType, TlamForAllType, TlamFunType, TlamTVarType),
  // Operations
  (VLambda, VReturn, TLambda, TReturn, TApply, VApply),
]
