package scair.dialects.tlam

import scair.ir.*
import scair.dialects.builtin.*
import scair.clair.macros.*
import scair.parse.*
import fastparse.ParsingRun
import fastparse.*
import scair.Printer

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

// !tlam.tvar<%s> - type referencing a SSA value %s
/*
final case class TlamTVarType(var tparam: Value[Attribute])
    extends TypeAttribute,
      ParametrizedAttribute:
  override def name = "tlam.tvar"
  override def parameters = Seq()

  override def customPrint(p: Printer): Unit =
    p.print("!tlam.tvar<")
    p.print(tparam)
    p.print(">")

given AttributeCompanion[TlamTVarType]:
  override def name = "tlam.tvar"

  override def parse[$: P](using Parser): P[TlamTVarType] =
    import scair.parse.whitespace
    P("<" ~ operandNameP.flatMap(operandP(_, TlamTypeType())) ~ ">")
      .map(v => TlamTVarType(v))
 */

// !tlam.tvar<%x> - type referencing an SSA value %x
final case class TlamTVarType(var tparam: Value[Attribute])
    extends TypeAttribute,
      ParametrizedAttribute:

  override def name: String = "tlam.tvar"

  override def parameters = Seq(tparam)

given AttributeCompanion[TlamTVarType]:
  override def name: String = "tlam.tvar"

  override def parse[$: P](using p: Parser): P[TlamTVarType] =
    valueRefInAnglesP(TlamTypeType()).map(TlamTVarType(_))

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
    with DerivedAttribute["tlam.forall", TlamForAllType]
    derives DerivedAttributeCompanion

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
