package scair.dialects.dlam

import scair.ir.*
import scair.clair.macros.*

val DlamDialect = summonDialect[
  // Custom attributes
  (DlamTypeType, DlamBVarType, DlamForAllType),
  // Operations
  (VLambda, VReturn, TLambda, TReturn, TApply, VApply)
](Seq(DlamFunType, DepType, DlamTVarType))
