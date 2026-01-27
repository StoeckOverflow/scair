package scair.dialects.tlam

import scair.ir.*
import scair.clair.macros.*
import scair.dialects.tlam.verify.DeBruijnIndicesCheck

val TlamDialect = summonDialect[
  // Custom attributes
  (TlamTypeType, TlamBVarType, TlamForAllType, TlamFunType, TlamTVarType),
  // Operations
  (VLambda, VReturn, TLambda, TReturn, TApply, VApply),
](Seq(DeBruijnIndicesCheck))
