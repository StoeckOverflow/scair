package scair.analysis

import scair.ir.*

import scala.collection.mutable

/** Structural IR metrics collected by traversing parsed IR nodes.
  *
  * The output is intended for small machine-readable tooling hooks, not for
  * human-facing pretty-printing. `scair-opt --emit-ir-metrics` renders these
  * metrics as deterministic `key=value` lines.
  */
final case class IRMetrics(
    totalOps: Int,
    funcDefs: Int,
    blocks: Int,
    blockArgs: Int,
    opCounts: Map[String, Int],
):
  def opCount(name: String): Int = opCounts.getOrElse(name, 0)

  def toKeyValueLines: Seq[String] =
    val opNames = (IRMetrics.trackedOpNames ++ opCounts.keySet).toSeq.distinct.sorted
    Seq(
      "status=ok",
      s"total_ops=$totalOps",
      s"func_defs=$funcDefs",
      s"blocks=$blocks",
      s"block_args=$blockArgs",
    ) ++ opNames.map(name => s"op.$name=${opCount(name)}")

object IRMetrics:
  val trackedOpNames: Seq[String] = Seq(
    "builtin.module",
    "func.func",
    "llvm.func",
    "memref.alloc",
    "d_memref.alloc",
    "memref.reinterpret_cast",
    "d_memref.reinterpret_cast",
    "memref.subview",
    "d_memref.subview",
    "memref.extract_strided_metadata",
    "d_memref.extract_strided_metadata",
    "memref.load",
    "memref.store",
    "d_memref.load",
    "d_memref.store",
  )

  def collect(root: Operation): IRMetrics =
    val opCounts = mutable.Map.empty[String, Int].withDefaultValue(0)
    var totalOps = 0
    var blocks = 0
    var blockArgs = 0

    def visit(op: Operation): Unit =
      totalOps += 1
      opCounts(op.name) = opCounts(op.name) + 1

      op.regions.foreach { region =>
        region.blocks.foreach { block =>
          blocks += 1
          blockArgs += block.arguments.length
          block.operations.foreach(visit)
        }
      }

    visit(root)

    val frozenCounts = opCounts.toMap
    IRMetrics(
      totalOps = totalOps,
      funcDefs = frozenCounts.getOrElse("func.func", 0) + frozenCounts
        .getOrElse("llvm.func", 0),
      blocks = blocks,
      blockArgs = blockArgs,
      opCounts = frozenCounts,
    )
