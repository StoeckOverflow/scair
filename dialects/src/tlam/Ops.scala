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

  override def customVerify(): OK[Operation] =
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
    with NoMemoryEffect
    with IsTerminator derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    def enclosingVLambda(op: Operation): Option[VLambda] =
      var curRegion = op.containerBlock.flatMap(_.containerRegion)
      var curParent = curRegion.flatMap(_.containerOperation)
      while curParent.isDefined do
        curParent.get match
          case vl: VLambda => return Some(vl)
          case parent      =>
            curRegion = parent.containerBlock.flatMap(_.containerRegion)
            curParent = curRegion.flatMap(_.containerOperation)
      None

    enclosingVLambda(this) match
      case Some(vl) =>
        val expected = vl.res.typ.out
        if value.typ == expected then OK(this)
        else
          Err(
            s"vreturn: expected value type $expected from enclosing vlambda, got ${value.typ}"
          )
      case None =>
        Err("vreturn: must appear inside a tlam.vlambda body")

final case class TLambda(
    body: Region,
    res: Result[TlamForAllType],
) extends DerivedOperation["tlam.tlambda", TLambda]
    derives DerivedOperationCompanion:

  override def customVerify(): OK[Operation] =
    def isWithinTLambdaBody(op: Operation): Boolean =
      var curRegion = op.containerBlock.flatMap(_.containerRegion)
      var curParent = curRegion.flatMap(_.containerOperation)
      while curParent.isDefined do
        curParent.get match
          case _: TLambda => return true
          case parent     =>
            curRegion = parent.containerBlock.flatMap(_.containerRegion)
            curParent = curRegion.flatMap(_.containerOperation)
      false

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
          case Some(TReturn(ret)) =>
            // Close the SSA-bound type variable into a de Bruijn body to compare
            // with the stored forall type.
            val closed = TlamTypeUtil.closeUnder(tparam, ret.typ)
            if closed != res.typ.body then
              Err(
                s"tlambda: result body ${res.typ.body} != return type closed over binder $closed"
              )
            else if !isWithinTLambdaBody(this) && TlamTypeUtil.containsTVar(
                res.typ.body
              )
            then
              // Outside any tlambda body, forall types must not reference SSA tvars.
              Err(
                "tlambda: forall body contains free tvar outside any tlambda body"
              )
            else OK(this)
          case Some(other) =>
            Err(s"tlambda: last op must be tlam.treturn, got '${other.name}'")
          case None =>
            Err("tlambda: body block must not be empty (needs a terminator)")

      case _ =>
        Err("tlambda: must have exactly one block with one arg")

final case class TReturn(
    value: Value[TypeAttribute]
) extends DerivedOperation["tlam.treturn", TReturn]
    with NoMemoryEffect
    with IsTerminator derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    def enclosingTLambda(op: Operation): Option[TLambda] =
      var curRegion = op.containerBlock.flatMap(_.containerRegion)
      var curParent = curRegion.flatMap(_.containerOperation)
      while curParent.isDefined do
        curParent.get match
          case tl: TLambda => return Some(tl)
          case parent      =>
            curRegion = parent.containerBlock.flatMap(_.containerRegion)
            curParent = curRegion.flatMap(_.containerOperation)
      None

    enclosingTLambda(this) match
      case Some(_) => OK(this)
      case None    => Err("treturn: must appear inside a tlam.tlambda body")

final case class TApply(
    fun: Value[TlamForAllType],
    tyArg: TypeAttribute,
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.tapply", TApply]
    derives DerivedOperationCompanion:

  override def customVerify(): OK[Operation] =
    val funAny: Value[Attribute] = fun
    funAny.typ match
      case fa: TlamForAllType =>
        val inst = DBI.instantiate(fa, tyArg)
        if res.typ == inst then OK(this)
        else Err(s"tapply: result ${res.typ} != instantiated $inst")
      case other =>
        Err(s"tapply: operand must have !tlam.forall type, got $other")

final case class VApply(
    fun: Value[TlamFunType],
    arg: Value[TypeAttribute],
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam.vapply", VApply]
    derives DerivedOperationCompanion:

  override def customVerify(): OK[Operation] =
    val funAny: Value[Attribute] = fun
    funAny.typ match
      case TlamFunType(in, out) =>
        if arg.typ == in && res.typ == out then OK(this)
        else
          Err(
            s"vapply: expected arg $in and result $out, got ${arg.typ} and ${res.typ}"
          )
      case other =>
        Err(s"vapply: first operand must have !tlam.fun type, got $other")
