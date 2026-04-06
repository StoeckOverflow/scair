package scair.dialects.tlam_de_bruijn

import scair.*
import scair.ir.*
import scair.clair.*

val TlamDeBruijnDialect: Dialect = summonDialect[
  (tlamTypeType, tlamBVarType, tlamForAllType, tlamFunType),
  (VLambda, VReturn, TLambda, TReturn, TApply, VApply),
]
