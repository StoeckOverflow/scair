package scair.dialects.tlam

import scair.ir.*
import scair.clair.*

val TlamDialect = summonDialect[
  // Custom attributes
  (TlamTypeType, TlamBVarType, TlamForAllType, TlamFunType),
  // Operations
  (VLambda, VReturn, TLambda, TReturn, TApply, VApply),
]
