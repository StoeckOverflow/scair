package scair.dialects.dlam

import scair.ir.*
import scair.dialects.builtin.ModuleOp
import scair.Printer

object DlamPrinterUtils:

  def printModule(m: ModuleOp): Unit =
    val mvt = ModuleValueTable(m)
    val printer = Printer()
    printer.moduleValueTable = Some(mvt)
    printer.printTopLevel(m)
