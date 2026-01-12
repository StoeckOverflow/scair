package scair.dialects.tlam

import scair.ir.*
import scair.utils.*
import scair.clair.macros.*
import scair.clair.codegen.*

final case class VLambda(
    funAttr: TlamFunType,
    body: Region,
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.vlambda", VLambda]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    (funAttr, res.typ) match
      case (f @ TlamFunType(in, _), r) if r == f =>
        body.blocks match
          case Block(args, _) :: Nil
              if args.length == 1 && args.head.typ == in =>
            OK(this)
          case _ =>
            Err("vlambda: one block with one arg of input type required")
      case _ => Err("vlambda: result type must equal function type")

final case class VReturn(
    value: Value[TypeAttribute],
    expected: TypeAttribute,
) extends DerivedOperation["tlam.vreturn", VReturn]
    with IsTerminator derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    if value.typ == expected then OK(this)
    else Err("vreturn: type mismatch")

final case class TLambda(
    tBody: Region,
    res: Result[TlamForAllType],
) extends DerivedOperation["tlam.tlambda", TLambda]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    val okBinder = tBody.blocks match
      case Block(args, _) :: Nil =>
        args.length == 1 && args.head.typ.isInstanceOf[TlamTypeType]
      case _ => false
    val okRes = res.typ.isInstanceOf[TlamForAllType]
    if okBinder && okRes then OK(this)
    else
      Err(
        "tlambda: one block with one tlam type arg and forall result required"
      )

final case class TReturn(
    value: Value[TypeAttribute],
    expected: TypeAttribute,
) extends DerivedOperation["tlam.treturn", TReturn]
    with IsTerminator derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    if value.typ == expected then OK(this)
    else Err("treturn: type mismatch")

final case class TApply(
    polymorphicFun: Value[TlamForAllType],
    argType: TypeAttribute,
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.tapply", TApply]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    polymorphicFun.typ match
      case fa @ TlamForAllType(_) =>
        val inst =
          DBI.instantiate(fa, argType)
        if res.typ == inst then OK(this)
        else Err(s"tapply: result ${res.typ} != instantiated $inst")

final case class VApply(
    fun: Value[TypeAttribute],
    arg: Value[TypeAttribute],
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.vapply", VApply]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    fun.typ match
      case TlamFunType(in, out) =>
        if arg.typ == in && res.typ == out then OK(this)
        else
          Err(
            s"vapply: expected arg $in and result $out, got ${arg.typ} and ${res
                .typ}"
          )
      case other => Err(s"vapply: fun not a function type: $other")
