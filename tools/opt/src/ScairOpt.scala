package scair.tools.opt

import scair.analysis.IRMetrics
import scair.exceptions.VerifyException
import scair.ir.*
import scair.parse.*
import scair.print.AssemblyPrinter
import scair.print.ErrorPrinter
import scair.passes.analysis.SizeProductFacts.FactorSelectionPolicy
import scair.passes.context_band_tiling.DependentContextBandExactTile
import scair.passes.context_band_tiling.DependentContextBandFactorTileWithTail
import scair.passes.context_band_tiling.DependentContextBandTileWithTail
import scair.passes.context_band_tiling.OrdinaryAffineContextBandTileWithTail
import scair.passes.dependent_size_product_loop_factorization.DependentSizeProductLoopFactorization
import scair.passes.dependent_size_product_tiling.DependentExactTile
import scair.passes.dependent_size_product_tiling.DependentProductLoopExactTile
import scair.passes.dependent_size_product_tiling.DependentTileWithTailControl
import scair.passes.dependent_size_product_tiling.OrdinaryAffineProductTileWithTail
import scair.passes.dependent_size_product_tiling.OrdinaryAffineProductLoopTileWithTail
import scair.tools.ScairToolBase
import scair.utils.*
import scair.verify.Verifier
import scair.dialects.tlam.verify.DeBruijnIndicesCheck
import scair.dialects.tlam_de_bruijn.verify.TlamDeBruijnIndicesCheck
import scopt.OParser

import scala.io.BufferedSource
import scala.io.Source
//
// ░██████╗ ░█████╗░ ░█████╗░ ██╗ ██████╗░
// ██╔════╝ ██╔══██╗ ██╔══██╗ ██║ ██╔══██╗
// ╚█████╗░ ██║░░╚═╝ ███████║ ██║ ██████╔╝
// ░╚═══██╗ ██║░░██╗ ██╔══██║ ██║ ██╔══██╗
// ██████╔╝ ╚█████╔╝ ██║░░██║ ██║ ██║░░██║
// ╚═════╝░ ░╚════╝░ ╚═╝░░╚═╝ ╚═╝ ╚═╝░░╚═╝
//
// ░█████╗░ ██████╗░ ████████╗
// ██╔══██╗ ██╔══██╗ ╚══██╔══╝
// ██║░░██║ ██████╔╝ ░░░██║░░░
// ██║░░██║ ██╔═══╝░ ░░░██║░░░
// ╚█████╔╝ ██║░░░░░ ░░░██║░░░
// ░╚════╝░ ╚═╝░░░░░ ░░░╚═╝░░░
//

case class ScairOptArgs(
    val allowUnregistered: Boolean = false,
    val emitIrMetrics: Boolean = false,
    val input: Option[String] = None,
    val skipVerify: Boolean = false,
    val splitInputFile: Boolean = false,
    val parsingDiagnostics: Boolean = false,
    val printGeneric: Boolean = false,
    val passes: Seq[String] = Seq(),
    val verifyDiagnostics: Boolean = false,
)

trait ScairOptBase extends ScairToolBase[ScairOptArgs]:

  override def dialects = scair.dialects.allDialects

  override def passes = scair.passes.allPasses

  override def parse(args: ScairOptArgs)(
      input: BufferedSource
  ): Array[OK[Operation]] =
    // TODO: more robust separator splitting
    val inputChunks =
      if args.splitInputFile then input.mkString.split("\n// -----\n")
      else Array(input.mkString)
    var indexOffset = 0
    inputChunks.map(input =>
      // Parse content
      val parser = new Parser(
        ctx,
        inputPath = args.input,
        parsingDiagnostics = args.parsingDiagnostics,
        allowUnregisteredDialect = args.allowUnregistered,
      )
      val parsed = parser.parse(
        input,
        parser = moduleP(using _, parser),
      ) match
        case fastparse.Parsed.Success(inputModule, _) =>
          OK(inputModule)
        case failure: fastparse.Parsed.Failure =>
          Err(parser.error(failure, indexOffset))
      if args.splitInputFile && !(input eq inputChunks.last) then
        indexOffset += input.count(_ == '\n') + 2

      parsed
    )

  private def verifyWithChecks(op: Operation): OK[Operation] =
    val checks =
      Seq(DeBruijnIndicesCheck, TlamDeBruijnIndicesCheck) ++
        Verifier.defaultChecks
    Verifier.verify(op, checks)

  override def parseArgs(args: Array[String]): ScairOptArgs =
    // Define CLI args
    val argbuilder = OParser.builder[ScairOptArgs]
    val argparser =
      import argbuilder.*
      OParser.sequence(
        commonHeaders,
        // The input file - defaulting to stdin
        arg[String]("file").optional().text("input file")
          .action((x, c) => c.copy(input = Some(x))),
        opt[Unit]('a', "allow-unregistered-dialect").optional().text(
          "Accept unregistered operations and attributes, bestPRINT effort with generic syntax."
        ).action((_, c) => c.copy(allowUnregistered = true)),
        opt[Unit]("emit-ir-metrics").optional().text(
          "Print parser-backed structural IR metrics as key=value lines instead of IR."
        ).action((_, c) => c.copy(emitIrMetrics = true)),
        opt[Unit]('s', "skip-verify").optional().text("Skip verification")
          .action((_, c) => c.copy(skipVerify = true)),
        opt[Unit]("split-input-file").optional()
          .text("Split input file on `// -----`")
          .action((_, c) => c.copy(splitInputFile = true)),
        opt[Unit]("parsing-diagnostics").optional().text(
          "Parsing diagnose mode, i.e parse errors are not fatal for the whole run"
        ).action((_, c) => c.copy(parsingDiagnostics = true)),
        opt[Unit]('g', "print-generic").optional()
          .text("Print Strictly in Generic format")
          .action((_, c) => c.copy(printGeneric = true)),
        opt[Seq[String]]('p', "passes").optional()
          .text("Specify passes to apply to the IR")
          .action((x, c) => c.copy(passes = x)),
        opt[Unit]("verify-diagnostics").optional().text(
          "Verification diagnose mode, i.e verification errors are not fatal for the whole run"
        ).action((_, c) => c.copy(verifyDiagnostics = true)),
      )

    // Parse the CLI args
    OParser.parse(argparser, args, ScairOptArgs()).get

  def handleVerificationError(
      error: Err,
      operation: Operation,
      verifyDiagnostics: Boolean,
  ): OK[Operation] =
    error match
      case Err(msg, Some(_)) =>
        val p = new ErrorPrinter(error)
        p.print(operation)
        if verifyDiagnostics then error else sys.exit(42)
      case Err(msg, None) =>
        Console.err.println(msg)
        if verifyDiagnostics then error else sys.exit(42)

  def main(args: Array[String]): Unit =

    val parsedArgs = parseArgs(args)

    // Open the input file or stdin
    val input = parsedArgs.input match
      case Some(file) => Source.fromFile(file)
      case None       => Source.stdin

    val parsedModules = parse(parsedArgs)(input)

    parsedModules.foreach(parsedModule =>

      parsedModule match
        case OK(inputModule) =>

          val processedModule: OK[Operation] =
            var module =
              if parsedArgs.skipVerify then OK(inputModule)
              else inputModule.structured.flatMap(op => verifyWithChecks(op))
            // verify parsed content
            module match
              case OK(op) =>
                // apply the specified passes
                parsedArgs.passes.foldLeft(module)((module, parsedPass) =>
                  val ordinaryAffinePrefix = "ordinary-affine-product-tile-with-tail:"
                  val ordinaryAffineAnyLoopPrefix = "ordinary-affine-product-loop-tile-with-tail:"
                  val ordinaryContextPrefix = "ordinary-affine-context-band-tile-with-tail:"
                  val dependentContextPrefix = "dependent-context-band-tile-with-tail:"
                  val dependentContextFactorPrefix = "dependent-context-band-factor-tile-with-tail:"
                  val dependentContextExactPrefix = "dependent-context-band-exact-tile:"
                  val dependentTileWithTailPrefix = "dependent-tile-with-tail-control:"
                  val dependentProductExactPrefix = "dependent-product-loop-exact-tile:"
                  val dependentExactPrefix = "dependent-exact-tile:"
                  val dependentFactorizationPrefix = "dependent-size-product-loop-factorization:"
                  def parsePositiveTileSize(passName: String, tileSizeText: String): BigInt =
                    if !tileSizeText.matches("[1-9][0-9]*") then
                      Console.err.println(
                        s"error: $passName expects a positive integer tile size, got '$tileSizeText'."
                      )
                      sys.exit(1)
                    BigInt(tileSizeText)

                  def parseFactorPolicy(passName: String, policyText: String): FactorSelectionPolicy =
                    if policyText == "rightmost-positive" then FactorSelectionPolicy.RightmostPositive
                    else if policyText == "leftmost-positive" then FactorSelectionPolicy.LeftmostPositive
                    else if policyText.startsWith("factor-index=") then
                      val indexText = policyText.stripPrefix("factor-index=")
                      if !indexText.matches("[0-9]+") then
                        Console.err.println(
                          s"error: $passName expects factor-index=N with non-negative integer N, got '$policyText'."
                        )
                        sys.exit(1)
                      FactorSelectionPolicy.FactorIndex(indexText.toInt)
                    else
                      Console.err.println(
                        s"error: $passName expects factor policy rightmost-positive, leftmost-positive, or factor-index=N, got '$policyText'."
                      )
                      sys.exit(1)

                  val pass =
                    if parsedPass.startsWith(ordinaryAffineAnyLoopPrefix) then
                      val tileSizeText = parsedPass.stripPrefix(ordinaryAffineAnyLoopPrefix)
                      OrdinaryAffineProductLoopTileWithTail(
                        ctx,
                        parsePositiveTileSize("ordinary-affine-product-loop-tile-with-tail", tileSizeText),
                      )
                    else if parsedPass.startsWith(ordinaryAffinePrefix) then
                      val tileSizeText = parsedPass.stripPrefix(ordinaryAffinePrefix)
                      OrdinaryAffineProductTileWithTail(
                        ctx,
                        parsePositiveTileSize("ordinary-affine-product-tile-with-tail", tileSizeText),
                      )
                    else if parsedPass.startsWith(ordinaryContextPrefix) then
                      val tileSizeText = parsedPass.stripPrefix(ordinaryContextPrefix)
                      OrdinaryAffineContextBandTileWithTail(
                        ctx,
                        parsePositiveTileSize("ordinary-affine-context-band-tile-with-tail", tileSizeText),
                      )
                    else if parsedPass.startsWith(dependentContextPrefix) then
                      val tileSizeText = parsedPass.stripPrefix(dependentContextPrefix)
                      DependentContextBandTileWithTail(
                        ctx,
                        parsePositiveTileSize("dependent-context-band-tile-with-tail", tileSizeText),
                      )
                    else if parsedPass.startsWith(dependentContextFactorPrefix) then
                      val policyText = parsedPass.stripPrefix(dependentContextFactorPrefix)
                      DependentContextBandFactorTileWithTail(
                        ctx,
                        parseFactorPolicy("dependent-context-band-factor-tile-with-tail", policyText),
                      )
                    else if parsedPass.startsWith(dependentContextExactPrefix) then
                      val policyText = parsedPass.stripPrefix(dependentContextExactPrefix)
                      DependentContextBandExactTile(
                        ctx,
                        parseFactorPolicy("dependent-context-band-exact-tile", policyText),
                      )
                    else if parsedPass.startsWith(dependentTileWithTailPrefix) then
                      val policyText = parsedPass.stripPrefix(dependentTileWithTailPrefix)
                      DependentTileWithTailControl(
                        ctx,
                        parseFactorPolicy("dependent-tile-with-tail-control", policyText),
                      )
                    else if parsedPass.startsWith(dependentProductExactPrefix) then
                      val policyText = parsedPass.stripPrefix(dependentProductExactPrefix)
                      DependentProductLoopExactTile(
                        ctx,
                        parseFactorPolicy("dependent-product-loop-exact-tile", policyText),
                      )
                    else if parsedPass.startsWith(dependentExactPrefix) then
                      val policyText = parsedPass.stripPrefix(dependentExactPrefix)
                      DependentExactTile(
                        ctx,
                        parseFactorPolicy("dependent-exact-tile", policyText),
                      )
                    else if parsedPass.startsWith(dependentFactorizationPrefix) then
                      val policyText = parsedPass.stripPrefix(dependentFactorizationPrefix)
                      DependentSizeProductLoopFactorization(
                        ctx,
                        parseFactorPolicy("dependent-size-product-loop-factorization", policyText),
                      )
                    else ctx.passContext.get(parsedPass) match
                    case Some(pass) => pass
                    case None       =>
                      Console.err.println(
                        f"error: '$parsedPass' does not refer to a registered pass."
                      )
                      Console.err.println(f"Currently registered passes are:")
                      ctx.passContext.keysIterator
                        .foreach(p => Console.println(f"  - $p"))
                      sys.exit(1)
                  module.map { op =>
                    val out = pass.transform(op)

                    if !parsedArgs.skipVerify then
                      verifyWithChecks(out) match
                        case Err(errorMsg, _) =>
                          if parsedArgs.verifyDiagnostics then
                            Err(errorMsg + "\n")
                          else throw new VerifyException(errorMsg)
                        case _ => ()

                    out

                  }
                )
              case err: Err =>
                handleVerificationError(
                  err,
                  inputModule,
                  parsedArgs.verifyDiagnostics,
                )

          {
            if parsedArgs.emitIrMetrics then
              processedModule.fold(
                err => println(err.msg),
                op => IRMetrics.collect(op).toKeyValueLines.foreach(println),
              )
            else
              val printer = new AssemblyPrinter(parsedArgs.printGeneric)
              processedModule.fold(
                err => printer.print(err.msg),
                printer.printTopLevel,
              )
              printer.flush()
          }
        case Err(msg = errorMsg, obj = _) =>
          if parsedArgs.parsingDiagnostics then println(errorMsg)
          else throw new Exception(errorMsg)

      if parsedModule != parsedModules.last then println("// -----")
    )

object ScairOpt extends ScairOptBase:
  def toolName: String = "scair-opt"
