package scair.ir

import scala.collection.mutable
import scair.dialects.builtin.ModuleOp

// ██╗ ██████╗░
// ██║ ██╔══██╗
// ██║ ██████╔╝
// ██║ ██╔══██╗
// ██║ ██║░░██║
// ╚═╝ ╚═╝░░╚═╝

/*≡==--==≡≡≡≡==--=≡≡*\
||      ValueID     ||
\*≡==---==≡≡==---==≡*/

/** Stable identifier for an operation, expressed as a path of indices along
  * regions/blocks/ops starting from the module op.
  *
  * Example: List(0, 2, 5) ≈
  *   - region #0 of module
  *   - block #2 of that region
  *   - op #5 of that block
  *
  * For nested regions we just keep extending the path.
  */
final case class OpId(path: List[Int])

/** Stable identifier for a value result: (defining operation, result index). */
final case class SSAValueId(defOpId: OpId, resultIndex: Int)

/** A bidirectional mapping between SSA values and ValueIds for a given module.
  */
final class ModuleValueTable(val module: ModuleOp):

  private val valueToId = mutable.Map[Value[Attribute], SSAValueId]()
  private val idToValue = mutable.Map[SSAValueId, Value[Attribute]]()

  // Build the tables eagerly
  build()

  /** Get the ValueId for a given SSA value, if it is part of this module. */
  def idOf(v: Value[Attribute]): Option[SSAValueId] = valueToId.get(v)

  /** Get the SSA value for a given ValueId, if it still exists. */
  def valueOf(id: SSAValueId): Option[Value[Attribute]] = idToValue.get(id)

  /** Iterate all entries (useful for debugging or passes). */
  def allEntries: Iterable[(Value[Attribute], SSAValueId)] = valueToId

  // ---------------- internal ----------------

  private def build(): Unit =
    // Walk all ops in the module and assign OpIds + ValueIds
    def walkOp(op: Operation, opId: OpId): Unit =
      // Register all results of this op
      val results = op.results
      var i = 0
      while i < results.length do
        val v = results(i)
        val vid = SSAValueId(opId, i)
        valueToId(v) = vid
        idToValue(vid) = v
        i += 1

      // Recurse into regions contained in this op
      val regions = op.regions
      var rIdx = 0
      while rIdx < regions.length do
        walkRegion(regions(rIdx), parentPath = opId.path :+ rIdx)
        rIdx += 1

    def walkRegion(region: Region, parentPath: List[Int]): Unit =
      val blocks = region.blocks
      var bIdx = 0
      while bIdx < blocks.length do
        val block = blocks(bIdx)

        // --- NEW: assign IDs to block arguments ---
        val blockHeaderId = OpId(parentPath :+ bIdx)
        val args = block.arguments
        var argIdx = 0
        while argIdx < args.length do
          val arg = args(argIdx)
          val vid = SSAValueId(blockHeaderId, argIdx)
          valueToId(arg) = vid
          idToValue(vid) = arg
          argIdx += 1

        val opsIter = block.operations.iterator
        var oIdx = 0
        while opsIter.hasNext do
          val op = opsIter.next()
          val opId = OpId(parentPath :+ bIdx :+ oIdx)
          walkOp(op, opId)
          oIdx += 1

        bIdx += 1

    // ModuleOp is the root; by convention its OpId is OpId(Nil)
    walkOp(module, OpId(Nil))

object ModuleValueTable:
  /** Convenience builder. */
  def apply(m: ModuleOp): ModuleValueTable = new ModuleValueTable(m)
