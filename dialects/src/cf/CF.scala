package scair.dialects.cf

import scair.clair.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

final case class Assert(
    arg: Operand[IntegerType],
    msg: StringData,
) extends DerivedOperation["cf.assert"] derives OpDefs:

  override def customVerify(): OK[Operation] =
    if arg.typ == I1 then OK(this)
    else Err(s"cf.assert: expected i1 condition, got ${arg.typ}")

val CFDialect = summonDialect[EmptyTuple, Tuple1[Assert]]
