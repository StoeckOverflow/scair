package scair.dialects.dlam

import scair.ir.*
import scair.clair.macros.*
import scair.clair.codegen.*

final case class VLambda(
    funAttr: DlamFunType,
    body: Region,
    res: Result[TypeAttribute]
) extends DerivedOperation["dlam.vlambda", VLambda]
    derives DerivedOperationCompanion:

  override def verify() =
    (funAttr, res.typ) match
      case (f @ DlamFunType(in, _), r) if r == f =>
        body.blocks match
          case Block(args, _) :: Nil
              if args.length == 1 && args.head.typ == in =>
            Right(this)
          case _ =>
            Left("vlambda: one block with one arg of input type required")
      case _ => Left("vlambda: result type must equal function type")

final case class VReturn(
    value: Value[TypeAttribute],
    expected: TypeAttribute
) extends DerivedOperation["dlam.vreturn", VReturn]
    with IsTerminator derives DerivedOperationCompanion:

  override def verify() =
    if value.typ == expected then Right(this)
    else Left("vreturn: type mismatch")

final case class TLambda(
    tBody: Region,
    res: Result[DlamForAllType]
) extends DerivedOperation["dlam.tlambda", TLambda]
    derives DerivedOperationCompanion:

  override def verify() =
    val okBinder = tBody.blocks match
      case Block(args, _) :: Nil =>
        args.length == 1 && args.head.typ.isInstanceOf[DlamTypeType]
      case _ => false
    val okRes = res.typ.isInstanceOf[DlamForAllType]
    if okBinder && okRes then Right(this)
    else
      Left(
        "tlambda: one block with one dlam type arg and forall result required"
      )

final case class TReturn(
    value: Value[TypeAttribute],
    expected: TypeAttribute
) extends DerivedOperation["dlam.treturn", TReturn]
    with IsTerminator derives DerivedOperationCompanion:

  override def verify() =
    if value.typ == expected then Right(this)
    else Left("treturn: type mismatch")

final case class TApply(
    polymorphicFun: Value[DlamForAllType],
    argType: TypeAttribute,
    res: Result[TypeAttribute]
) extends DerivedOperation["dlam.tapply", TApply]
    derives DerivedOperationCompanion:

  override def verify() =
    polymorphicFun.typ match
      case fa @ DlamForAllType(_) =>
        val inst =
          DBI.instantiate(fa, argType)
        if res.typ == inst then Right(this)
        else Left(s"tapply: result ${res.typ} != instantiated ${inst}")

final case class VApply(
    fun: Value[TypeAttribute],
    arg: Value[TypeAttribute],
    res: Result[TypeAttribute]
) extends DerivedOperation["dlam.vapply", VApply]
    derives DerivedOperationCompanion:

  override def verify() =
    fun.typ match
      case DlamFunType(in, out) =>
        if arg.typ == in && res.typ == out then Right(this)
        else
          Left(
            s"vapply: expected arg $in and result $out, got ${arg.typ} and ${res.typ}"
          )
      case other => Left(s"vapply: fun not a function type: $other")
