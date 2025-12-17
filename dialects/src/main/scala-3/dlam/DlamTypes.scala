package scair.dialects.dlam

import scair.ir.*
import scair.dialects.builtin.*
import scair.clair.macros.*
import scair.parse.*
import fastparse.ParsingRun
import fastparse.*
import scair.Printer

// ========================= Types (with de Bruijn) =========================

// A sealed "kind" for all dlam types
sealed trait DlamType extends TypeAttribute

// !dlam.type  — the universe of dlam types
final case class DlamTypeType()
    extends DlamType
    with DerivedAttribute["dlam.type", DlamTypeType]
    derives DerivedAttributeCompanion

// !dlam.bvar<k>  — De Bruijn index (k is data)
final case class DlamBVarType(k: IntegerAttr)
    extends DlamType
    with DerivedAttribute["dlam.bvar", DlamBVarType]
    derives DerivedAttributeCompanion

// !dlam.tvar<%s> - type refereencing a SSA value %s
final case class DlamTVarType(var expr: DepTypeExpr)
    extends TypeAttribute,
      ParametrizedAttribute:
  override def name = "dlam.tvar"
  override def parameters = Seq()

  override def customPrint(p: Printer): Unit =
    p.print("!dlam.tvar<")
    DepTypePrinter.print(expr, p)
    p.print(">")

given AttributeCompanion[DlamTVarType]:
  override def name = "dlam.tvar"

  override def parse[$: P](using Parser): P[DlamTVarType] =
    import DepTypeParser.*
    import scair.parse.whitespace
    P("<" ~ DepTypeExpr ~ ">").map(expr => DlamTVarType(expr))

// !dlam.fun<in -> out> — function type
final case class DlamFunType(in: TypeAttribute, out: TypeAttribute)
    extends ParametrizedAttribute(),
      DlamType:
  override def name: String = "dlam.fun"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(in, out)

given AttributeCompanion[DlamFunType]:
  override def name = "dlam.fun"

  override def parse[$: P](using Parser): P[DlamFunType] =
    P(
      "<" ~ typeP ~ "," ~ typeP ~ ">"
    ).map { (in, out) =>
      DlamFunType(
        in.asInstanceOf[TypeAttribute],
        out.asInstanceOf[TypeAttribute],
      )
    }

// !dlam.forall<body> — polymorphic type (body may reference bvar(0))
final case class DlamForAllType(body: TypeAttribute)
    extends DlamType
    with DerivedAttribute["dlam.forall", DlamForAllType]
    derives DerivedAttributeCompanion

object DlamTy:
  inline def `type`: DlamType = DlamTypeType()
  inline def bvar(k: IntData): DlamType = DlamBVarType(IntegerAttr(k, I64))

  inline def fun(in: TypeAttribute, out: TypeAttribute): DlamType =
    DlamFunType(in, out)

  inline def forall(body: TypeAttribute): DlamType = DlamForAllType(body)

/** \========================= de Bruijn utilities \=========================
  *   - shift(d, c, t) — increase indices >= c by d (used when entering/leaving
  *     binders)
  *   - subst(c, s, t) — substitute BVar(c) in t with s (capture-avoiding)
  */
object DBI:
  import DlamTy.*

  // shift(d, c, t): increase all indices >= c by d
  def shift(d: Int, c: Int, t: TypeAttribute): TypeAttribute = t match
    case DlamBVarType(IntegerAttr(k, t)) if k.data >= c =>
      bvar(IntData(k.data + d))
    case b @ DlamBVarType(_)  => b
    case DlamFunType(i, o)    => fun(shift(d, c, i), shift(d, c, o))
    case DlamForAllType(body) => forall(shift(d, c + 1, body))
    case other                => other

  // subst(c, s, t): substitute bvar(c) := s
  def subst(c: Int, s: TypeAttribute, t: TypeAttribute): TypeAttribute = t match
    case DlamBVarType(IntegerAttr(k, t)) if k.data == c => s
    case DlamBVarType(IntegerAttr(k, t)) if k.data > c  =>
      bvar(IntData(k.data - 1))
    case b @ DlamBVarType(_)  => b
    case DlamFunType(i, o)    => fun(subst(c, s, i), subst(c, s, o))
    case DlamForAllType(body) =>
      forall(subst(c + 1, shift(1, 0, s), body)) // or maybe shift by c?

    case other => other

  // instantiate ∀.body with arg
  def instantiate(fa: TypeAttribute, arg: TypeAttribute): TypeAttribute =
    fa match
      case DlamForAllType(body) => subst(0, arg, body)
      case other                => other
