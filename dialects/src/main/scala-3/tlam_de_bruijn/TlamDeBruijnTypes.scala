package scair.dialects.tlam_de_bruijn

import scair.ir.*
import scair.utils.*
import scair.dialects.builtin.*
import scair.clair.macros.*
import scair.parse.*
import fastparse.*

// ========================= Types (with de Bruijn) =========================

sealed trait tlamType extends TypeAttribute

final case class tlamTypeType()
    extends tlamType
    with DerivedAttribute["tlam.type", tlamTypeType]
    derives DerivedAttributeCompanion

final case class tlamBVarType(k: IntegerAttr)
    extends tlamType
    with DerivedAttribute["tlam.bvar", tlamBVarType]
    derives DerivedAttributeCompanion:

  override def customVerify(): OK[Unit] =
    if k.value < 0 then Err(s"tlam.bvar index must be >= 0, got ${k.value}")
    else OK(())

final case class tlamFunType(in: TypeAttribute, out: TypeAttribute)
    extends ParametrizedAttribute(),
      tlamType:
  override def name: String = "tlam.fun"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(in, out)

given AttributeCompanion[tlamFunType]:
  override def name = "tlam.fun"

  override def parse[$: P](using Parser): P[tlamFunType] =
    P("<" ~ typeP ~ "," ~ typeP ~ ">").map { (in, out) =>
      tlamFunType(
        in.asInstanceOf[TypeAttribute],
        out.asInstanceOf[TypeAttribute],
      )
    }

final case class tlamForAllType(body: TypeAttribute)
    extends tlamType
    with DerivedAttribute["tlam.forall", tlamForAllType]
    derives DerivedAttributeCompanion

object tlamTy:
  inline def `type`: tlamType = tlamTypeType()
  inline def bvar(k: IntData): tlamBVarType = tlamBVarType(IntegerAttr(k, I64))
  inline def fun(in: TypeAttribute, out: TypeAttribute): tlamFunType =
    tlamFunType(in, out)
  inline def forall(body: TypeAttribute): tlamForAllType = tlamForAllType(body)

/** De Bruijn utilities:
  *   - shift(d, c, t): increase indices >= c by d
  *   - subst(c, s, t): substitute BVar(c) in t with s (capture-avoiding)
  */
object DBI:
  import tlamTy.*

  def shift(d: Int, c: Int, t: TypeAttribute): TypeAttribute = t match
    case tlamBVarType(IntegerAttr(k, _)) if k.data >= c =>
      bvar(IntData(k.data + d))
    case b @ tlamBVarType(_)  => b
    case tlamFunType(i, o)    => fun(shift(d, c, i), shift(d, c, o))
    case tlamForAllType(body) => forall(shift(d, c + 1, body))
    case other                => other

  def subst(c: Int, s: TypeAttribute, t: TypeAttribute): TypeAttribute = t match
    case tlamBVarType(IntegerAttr(k, _)) if k.data == c => s
    case tlamBVarType(IntegerAttr(k, _)) if k.data > c  =>
      bvar(IntData(k.data - 1))
    case b @ tlamBVarType(_)  => b
    case tlamFunType(i, o)    => fun(subst(c, s, i), subst(c, s, o))
    case tlamForAllType(body) =>
      forall(subst(c + 1, shift(1, 0, s), body))
    case other => other

  def instantiate(fa: tlamForAllType, arg: TypeAttribute): TypeAttribute =
    subst(0, arg, fa.body)
