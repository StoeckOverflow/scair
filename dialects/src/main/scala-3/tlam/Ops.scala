package scair.dialects.tlam

import scair.ir.*
import scair.utils.*
import scair.clair.macros.*
import scair.clair.codegen.*

final case class VLambda(
    body: Region,
    res: Result[TlamFunType],
) extends DerivedOperation["tlam.vlambda", VLambda]
    derives DerivedOperationCompanion:

  /*
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
   */
  override def verify(): OK[Operation] =
    val funTy = res.typ
    body.blocks match
      case Block(args, ops) :: Nil
          if args.length == 1 && args.head.typ == funTy.in =>
        ops.lastOption match
          case Some(VReturn(ret)) =>
            if ret.typ == funTy.out then OK(this)
            else
              Err(s"vlambda: return type mismatch, expected ${funTy
                  .out}, got ${ret.typ}")
          case Some(other) =>
            Err(s"vlambda: last op must be tlam.vreturn, got '${other.name}'")
          case None =>
            Err("vlambda: body block must not be empty (needs a terminator)")
      case _ =>
        Err("vlambda: one block with one arg of input type required")

final case class VReturn(
    value: Value[TypeAttribute]
) extends DerivedOperation["tlam.vreturn", VReturn]
    with IsTerminator derives DerivedOperationCompanion

final case class TLambda(
    body: Region,
    res: Result[TlamForAllType],
) extends DerivedOperation["tlam.tlambda", TLambda]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    body.blocks match
      case Block(args, ops) :: Nil if args.length == 1 =>
        ops.lastOption match
          case Some(_: TReturn) => OK(this)
          case Some(other)      =>
            Err(s"tlambda: last op must be tlam.treturn, got '${other.name}'")
          case None =>
            Err("tlambda: body block must not be empty (needs a terminator)")
      case _ =>
        Err("tlambda: must have exactly one block with one arg")

  /*
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
   */

final case class TReturn(
    value: Value[TypeAttribute]
) extends DerivedOperation["tlam.treturn", TReturn]
    with IsTerminator derives DerivedOperationCompanion

final case class TApply(
    fun: Value[TlamForAllType],
    tyArg: TypeAttribute,
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.tapply", TApply]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    val inst = DBI.instantiate(fun.typ, tyArg)
    if res.typ == inst then OK(this)
    else Err(s"tapply: result ${res.typ} != instantiated $inst")

  /*
  override def verify(): OK[Operation] =
    polymorphicFun.typ match
      case fa @ TlamForAllType(_) =>
        val inst =
          DBI.instantiate(fa, argType)
        if res.typ == inst then OK(this)
        else Err(s"tapply: result ${res.typ} != instantiated $inst")
   */

final case class VApply(
    fun: Value[TlamFunType],
    arg: Value[TypeAttribute],
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.vapply", VApply]
    derives DerivedOperationCompanion:

  override def verify(): OK[Operation] =
    val TlamFunType(in, out) = fun.typ
    if arg.typ == in && res.typ == out then OK(this)
    else
      Err(
        s"vapply: expected arg $in and result $out, got ${arg.typ} and ${res.typ}"
      )

  /*
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
   */
