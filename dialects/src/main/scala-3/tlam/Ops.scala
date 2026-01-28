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
        val tparam = args.head

        // Binder arg must be a type value: %T : !tlam.type
        tparam.typ match
          case _: TlamTypeType => ()
          case other           =>
            return Err(
              s"tlambda: binder block argument must have type !tlam.type, got $other"
            )

        ops.lastOption match
          case Some(_: TReturn) => OK(this)
          case Some(other)      =>
            Err(s"tlambda: last op must be tlam.treturn, got '${other.name}'")
          case None =>
            Err("tlambda: body block must not be empty (needs a terminator)")

      case _ =>
        Err("tlambda: must have exactly one block with one arg")

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
