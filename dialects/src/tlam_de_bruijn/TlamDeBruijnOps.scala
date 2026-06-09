package scair.dialects.tlam_de_bruijn

import scair.ir.*
import scair.utils.*
import scair.clair.*

/** tlam.vlambda — value-level lambda abstraction. */
final case class VLambda(
    body: Region,
    res: Result[tlamFunType],
) extends DerivedOperation["tlam_dbi.vlambda"]
    derives OpDefs:

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
            Err(s"vlambda: last op must be tlam_dbi.vreturn, got '${other.name}'")
          case None =>
            Err("vlambda: body block must not be empty (needs a terminator)")
      case _ =>
        Err("vlambda: one block with one arg of input type required")

final case class VReturn(
    value: Value[TypeAttribute]
) extends DerivedOperation["tlam_dbi.vreturn"]
    with NoMemoryEffect
    with IsTerminator derives OpDefs:

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
            s"vreturn: expected value type $expected from enclosing tlam_dbi.vlambda, got ${value.typ}"
          )
      case None =>
        Err("vreturn: must appear inside a tlam_dbi.vlambda body")

/** tlam.tlambda — type-level lambda abstraction (forall introduction). */
final case class TLambda(
    body: Region,
    res: Result[tlamForAllType],
) extends DerivedOperation["tlam_dbi.tlambda"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    body.blocks match
      case Block(args, ops) :: Nil if args.isEmpty =>
        ops.lastOption match
          case Some(TReturn(ret)) =>
            val expected = res.typ.body
            if ret.typ == expected then OK(this)
            else
              Err(
                s"tlambda: return type mismatch, expected $expected, got ${ret
                    .typ}"
              )
          case Some(other) =>
            Err(s"tlambda: last op must be tlam_dbi.treturn, got '${other.name}'")
          case None =>
            Err("tlambda: body block must not be empty (needs a terminator)")
      case _ =>
        Err("tlambda: must have exactly one block with zero args")

final case class TReturn(
    value: Value[TypeAttribute]
) extends DerivedOperation["tlam_dbi.treturn"]
    with NoMemoryEffect
    with IsTerminator derives OpDefs:

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
      case None    => Err("treturn: must appear inside a tlam_dbi.tlambda body")

/** tlam.tapply — type application (forall elimination). */
final case class TApply(
    fun: Value[TypeAttribute],
    tyArg: Attribute,
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam_dbi.tapply"]
    with NoMemoryEffect
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    (fun.typ, tyArg) match
      case (fa: tlamForAllType, argTy: TypeAttribute) =>
        val inst = DBI.instantiate(fa, argTy)
        if res.typ == inst then OK(this)
        else Err(s"tapply: result ${res.typ} != instantiated $inst")
      case (_: tlamForAllType, other) =>
        Err(s"tapply: expected type argument, got $other")
      case (other, _) =>
        Err(s"tapply: expected operand of type tlam.forall, got $other")

/** tlam.vapply — value-level function application. */
final case class VApply(
    fun: Value[TypeAttribute],
    arg: Value[TypeAttribute],
    res: Result[TypeAttribute],
) extends DerivedOperation["tlam_dbi.vapply"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    fun.typ match
      case tlamFunType(in, out) =>
        if arg.typ == in && res.typ == out then OK(this)
        else
          Err(
            s"vapply: expected arg $in and result $out, got ${arg.typ} and ${res
                .typ}"
          )
      case other =>
        Err(s"vapply: expected callee of type tlam.fun<in,out>, got $other")
