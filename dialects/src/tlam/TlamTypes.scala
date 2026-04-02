package scair.dialects.tlam

import scair.ir.*
import scair.dialects.builtin.*
import scair.clair.macros.*
import scair.parse.*
import scair.utils.*
import fastparse.ParsingRun
import fastparse.*

// ========================= Types (with de Bruijn) =========================

// A sealed "kind" for all tlam types
sealed trait TlamType extends TypeAttribute

// !tlam.type  — the universe of tlam types
final case class TlamTypeType()
    extends TlamType
    with DerivedAttribute["tlam.type", TlamTypeType]
    derives DerivedAttributeCompanion

// !tlam.bvar<k>  — De Bruijn index (k is data)
final case class TlamBVarType(k: IntegerAttr)
    extends TlamType
    with DerivedAttribute["tlam.bvar", TlamBVarType]
    derives DerivedAttributeCompanion

// !tlam.fun<in -> out> — function type
final case class TlamFunType(in: TypeAttribute, out: TypeAttribute)
    extends ParametrizedAttribute(),
      TlamType:
  override def name: String = "tlam.fun"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(in, out)

given AttributeCompanion[TlamFunType]:
  override def name = "tlam.fun"

  override def parse[$: P](using Parser): P[TlamFunType] =
    P(
      "<" ~ typeP ~ "," ~ typeP ~ ">"
    ).map { (in, out) =>
      TlamFunType(
        in.asInstanceOf[TypeAttribute],
        out.asInstanceOf[TypeAttribute],
      )
    }

// !tlam.forall<body> — polymorphic type (body may reference bvar(0))
final case class TlamForAllType(body: TypeAttribute)
    extends TlamType
    with ParametrizedAttribute:
  override def name: String = "tlam.forall"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(body)

given AttributeCompanion[TlamForAllType]:
  override def name = "tlam.forall"

  override def parse[$: P](using Parser): P[TlamForAllType] =
    P("<" ~ typeP ~ ">")
      .map(body => TlamForAllType(body.asInstanceOf[TypeAttribute]))

object TlamTy:
  inline def `type`: TlamType = TlamTypeType()
  inline def bvar(k: IntData): TlamBVarType = TlamBVarType(IntegerAttr(k, I64))

  inline def fun(in: TypeAttribute, out: TypeAttribute): TlamFunType =
    TlamFunType(in, out)

  inline def forall(body: TypeAttribute): TlamForAllType =
    TlamForAllType(body)

/** \========================= de Bruijn utilities \=========================
  *   - shift(d, c, t) — increase indices >= c by d (used when entering/leaving
  *     binders)
  *   - subst(c, s, t) — substitute BVar(c) in t with s (capture-avoiding)
  */

object DBI:
  import TlamTy.*

  // shift(d, c, t): increase all indices >= c by d
  def shift(d: Int, c: Int, t: TypeAttribute): TypeAttribute = t match
    case TlamBVarType(IntegerAttr(k, t)) if k.data >= c =>
      bvar(IntData(k.data + d))
    case b @ TlamBVarType(_)  => b
    case TlamFunType(i, o)    => fun(shift(d, c, i), shift(d, c, o))
    case TlamForAllType(body) => forall(shift(d, c + 1, body))
    case other                => other

  // subst(c, s, t): substitute bvar(c) := s
  def subst(c: Int, s: TypeAttribute, t: TypeAttribute): TypeAttribute = t match
    case TlamBVarType(IntegerAttr(k, t)) if k.data == c => s
    case TlamBVarType(IntegerAttr(k, t)) if k.data > c  =>
      bvar(IntData(k.data - 1))
    case b @ TlamBVarType(_)  => b
    case TlamFunType(i, o)    => fun(subst(c, s, i), subst(c, s, o))
    case TlamForAllType(body) =>
      forall(subst(c + 1, shift(1, 0, s), body))
    case other => other

  // instantiate forAll.body with arg
  def instantiate(fa: TlamForAllType, arg: TypeAttribute): TypeAttribute =
    subst(0, arg, fa.body)

object TlamTypeUtil:
  import TlamTy.*

  final case class CloseResult(
      closed: TypeAttribute,
      containsFreeTVar: Boolean,
  )

  private def closeAndDetect(
      binder: Value[Attribute],
      depth: Int,
      t: TypeAttribute,
  ): CloseResult =
    t match
      case tv: ValueRefType =>
        if tv.value eq binder then
          CloseResult(
            closed = bvar(IntData(depth)),
            containsFreeTVar = false,
          )
        else
          CloseResult(
            closed = tv,
            containsFreeTVar = true,
          )

      case TlamFunType(in, out) =>
        val inR = closeAndDetect(binder, depth, in)
        val outR = closeAndDetect(binder, depth, out)
        CloseResult(
          closed = fun(inR.closed, outR.closed),
          containsFreeTVar = inR.containsFreeTVar || outR.containsFreeTVar,
        )

      case TlamForAllType(body) =>
        val bodyR = closeAndDetect(binder, depth + 1, body)
        CloseResult(
          closed = forall(bodyR.closed),
          containsFreeTVar = bodyR.containsFreeTVar,
        )

      case other =>
        CloseResult(
          closed = other,
          containsFreeTVar = false,
        )

  def containsTVar(t: TypeAttribute): Boolean = t match
    case _: ValueRefType   => true
    case TlamFunType(i, o) => containsTVar(i) || containsTVar(o)
    case TlamForAllType(b) => containsTVar(b)
    case _                 => false

  def closeUnder(
      binder: Value[Attribute],
      t: TypeAttribute,
  ): TypeAttribute =
    closeAndDetect(binder, depth = 0, t).closed

  def closeUnderAndContainsFreeTVar(
      binder: Value[Attribute],
      t: TypeAttribute,
  ): CloseResult =
    closeAndDetect(binder, depth = 0, t)
