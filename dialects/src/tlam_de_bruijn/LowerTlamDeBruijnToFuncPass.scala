package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.{InsertPoint, RewriteMethods}
import scair.transformations.*
import scair.transformations.patterns.*
import scair.dialects.func.*
import scair.dialects.builtin.*
import scair.dialects.tlam_de_bruijn.*
import scala.collection.mutable

final class LowerTlamDeBruijnToFuncPass(ctx: MLContext)
    extends ModulePass(ctx):
  override val name = "lower-tlam-de-bruijn-to-func"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        lower(m); m
      case other => other

  private def hasTypeLevelTLam(m: ModuleOp): Boolean =
    var found = false
    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        b.operations.foreach { op =>
          op match
            case _: TLambda | _: TApply | _: TReturn =>
              found = true
            case _ => ()
          if !found then op.regions.foreach(walkRegion)
        }
      }
    m.regions.foreach(walkRegion)
    found

  private def lower(m: ModuleOp): Unit =
    // Lowering assumes type-level TLam control has already been erased.
    // If not, leave unchanged and let verifier/pipeline staging report issues.
    if hasTypeLevelTLam(m) then return

    val top = m.regions.head.blocks.head
    var counter = 0
    val usedSymbolNames: mutable.Set[String] =
      mutable.Set.from(
        top.operations.collect { case s: Symbol => s.sym_name.stringLiteral }
      )

    def freshLiftedName(): String =
      var name = ""
      var found = false
      while !found do
        counter += 1
        val candidate = s"lifted_$counter"
        if !usedSymbolNames.contains(candidate) then
          name = candidate
          usedSymbolNames += candidate
          found = true
      name

    /** Lower a TLam function type to a builtin FunctionType. */
    def lowerFunType(ft: tlamFunType): FunctionType =
      // Note: inputs/outputs can still be TLam types (or builtin types). That's fine.
      FunctionType(inputs = Seq(ft.in), outputs = Seq(ft.out))

    // ---------------------------
    // Phase 1: lift every VLambda
    //   - create func.func @lifted_n with MOVED body
    //   - create func.constant @lifted_n : builtin.function_type<...>
    //   - replace uses of vl.res with constant result
    //   - erase original VLambda
    // ---------------------------
    def walkRegion(r: Region): Unit =
      r.blocks.foreach { b =>
        // snapshot, because we'll insert/erase while iterating
        val opsSnapshot = b.operations.toList
        opsSnapshot.foreach { op =>
          op match
            case vl: VLambda =>
              val name = freshLiftedName()

              val tlamFT: tlamFunType = vl.res.typ
              val fnTy: FunctionType = lowerFunType(tlamFT)

              // Move body into func.func (safe because we erase vl immediately).
              val bodyMoved: Region = vl.body.detached

              val fn = Func(
                sym_name = StringData(name),
                function_type = fnTy,
                sym_visibility = None,
                body = bodyMoved,
              )

              // Materialize a first-class function value (builtin FunctionType)
              val cst = Constant(
                value = SymbolRefAttr(name),
                res = Result(fnTy),
              )

              // Materialize the constant at module top so it dominates all
              // rewritten uses, including across nested regions.
              RewriteMethods.insertOpsAt(InsertPoint.atStartOf(top), cst)

              // Insert the function at module top. Inserting it after the
              // constant keeps the final module order as func then constant.
              RewriteMethods.insertOpsAt(InsertPoint.atStartOf(top), fn)

              // Replace all uses of the lambda value with the constant value
              // (upcast to Attribute to match helper signature)
              RewriteMethods.replaceValue(vl.res, cst.res)

              // Erase the original VLambda
              RewriteMethods.eraseOp(vl)

            case _ => ()

          op.regions.foreach(walkRegion)
        }
      }

    walkRegion(m.regions.head)

    // ---------------------------
    // Phase 2: rewrite remaining tlam value-level ops
    //   - vapply  -> func.call_indirect
    //   - vreturn -> func.return
    // ---------------------------
    val p = GreedyRewritePatternApplier(
      Seq(
        pattern { case app: VApply =>
          // IMPORTANT:
          // app.fun is statically Value[TlamFunType], but after Phase 1 its *runtime*
          // Value.typ can be builtin FunctionType (because we replaced uses with func.constant).
          val funV: Value[Attribute] = app.operands.head // widen for runtime inspection
          val argV: Value[Attribute] = app.operands(1)
          val ft: FunctionType =
            funV.typ match
              case f: FunctionType => f
              case other           =>
                throw new Exception(
                  s"lower-tlam-to-func: expected callee of call_indirect to have builtin.function_type, got $other"
                )

          CallIndirect(
            callee = funV.asInstanceOf[Operand[FunctionType]],
            callee_operands = Seq(argV), // arguments only
            _results = ft.outputs
              .map(Result(_)), // result types from function type
          )
        },
        pattern { case vr: VReturn =>
          Return(Seq(vr.operands.head))
        },
      )
    )

    PatternRewriteWalker(p).rewrite(m)
