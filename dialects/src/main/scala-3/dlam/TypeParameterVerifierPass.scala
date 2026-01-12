package scair.passes

import scair.MLContext
import scair.ir.*
import scair.dialects.builtin.*
import scair.transformations.*
import scair.dialects.dlam.*

class TypeParameterVerifierPass(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "verify-type-params"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        verifyModule(m)
        m
      case _ =>
        op

  // ----------------- top-level walk -----------------

  private def verifyModule(m: ModuleOp): Unit =
    def walkOp(o: Operation): Unit =
      checkOpTypes(o)
      o.regions.foreach(walkRegion)

    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.arguments.foreach(arg => checkTypeAttr(arg.typ, None))
        b.operations.foreach(walkOp)
      }

    walkOp(m)

  // ----------------- per-op checks -----------------

  private def checkOpTypes(o: Operation): Unit =
    o.results.foreach(r => checkTypeAttr(r.typ, Some(o)))
    o.operands.foreach(v => checkTypeAttr(v.typ, Some(o)))
    o.attributes.values.foreach {
      case tv: DlamTVarType => checkTVarType(tv, Some(o))
      case _                => ()
    }

  private def checkTypeAttr(t: Attribute, useSite: Option[Operation]): Unit =
    t match
      case tv: DlamTVarType => checkTVarType(tv, useSite)
      case _                => ()

  private def checkTVarType(
      tv: DlamTVarType,
      useSite: Option[Operation],
  ): Unit =
    checkValueDominance(tv.tparam, useSite)

// ----------------- Dominance logic -----------------

private def checkValueDominance(
    v: Value[Attribute],
    useSite: Option[Operation],
): Unit =
  useSite.foreach { user =>
    // Structural integrity checks (helpful after transformations)
    if user.containerBlock.isEmpty then
      throw new Exception(
        s"IR malformed: use-site operation ${user.name} has no containerBlock"
      )

    if v.owner.isEmpty then
      throw new Exception(
        s"IR malformed: value referenced from type has no owner: $v"
      )

    // Semantic dominance
    if !isDominated(v, user) then
      throw new Exception(
        s"Type parameter not dominated by its definition: tparam=$v, user=${user
            .name}"
      )
  }

/** Lexical dominance for Stage 1:
  *   - block args dominate only within their lexical subtree
  *   - op results: same-block def-before-use, otherwise lexical ancestry
  *
  * Fail-closed: if ownership / containment info is missing, return false.
  */
private def isDominated(v: Value[Attribute], user: Operation): Boolean =
  val vOwnerOpt = v.owner
  val userBlockOpt = user.containerBlock

  // If the user is not even attached to a block, the IR is broken.
  if userBlockOpt.isEmpty then return false

  vOwnerOpt match
    case Some(defBlock: Block) =>
      // Block arguments dominate only within their lexical subtree
      defBlock.isAncestor(user)

    case Some(defOp: Operation) =>
      defOp.containerBlock match
        // If the defining op is not attached to a block, the IR is broken.
        case None           => false
        case Some(defBlock) =>
          val userBlock = userBlockOpt.get
          if defBlock eq userBlock then
            // Same block: require def-before-use
            val ops = userBlock.operations.toSeq
            val defIx = ops.indexOf(defOp)
            val useIx = ops.indexOf(user)
            defIx >= 0 && useIx >= 0 && defIx <= useIx
          else
            // Different blocks: require lexical ancestry
            defOp.isAncestor(user)

    // Value has no owner => cannot be dominated in a meaningful way
    case None =>
      false
