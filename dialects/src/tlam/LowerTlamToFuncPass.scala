package scair.passes

import scair.MLContext
import scair.ir.*
import scair.transformations.{InsertPoint, RewriteMethods}
import scair.transformations.{GreedyRewritePatternApplier, ModulePass, PatternRewriteWalker, pattern}
import scair.dialects.func.*
import scair.dialects.builtin.*
import scair.dialects.tlam.*
import scala.collection.mutable

final class LowerTLamToFuncPass(ctx: MLContext) extends ModulePass(ctx):
  override val name = "lower-tlam-to-func"

  override def transform(op: Operation): Operation =
    op match
      case m: ModuleOp =>
        lower(m); m
      case other => other

  private def lower(m: ModuleOp): Unit =
    var counter = 0
    val top = m.regions.head.blocks.head
    val usedSymbolNames: mutable.Set[String] =
      mutable.Set
        .from(
          top.operations.collect { case s: Symbol => s.sym_name.stringLiteral }
        )

    def freshLiftedName(): String =
      var candidate = ""
      var unique = false
      while !unique do
        counter += 1
        candidate = s"lifted_$counter"
        unique = !usedSymbolNames.contains(candidate)
      usedSymbolNames += candidate
      candidate

    /** Lower a TLam function type to a builtin FunctionType. */
    def lowerFunType(ft: TlamFunType): FunctionType =
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

              val tlamFT: TlamFunType = vl.res.typ
              val fnTy: FunctionType = lowerFunType(tlamFT)

              // Move body into func.func (safe because we erase vl immediately).
              val bodyMoved: Region = vl.body.detached

              val fn = Func(
                sym_name = StringData(name),
                function_type = fnTy,
                sym_visibility = None,
                body = bodyMoved,
              )

              // Insert the function at module top
              RewriteMethods.insertOpsAt(InsertPoint.atStartOf(top), fn)

              // Materialize a first-class function value (builtin FunctionType)
              val cst = Constant(
                value = SymbolRefAttr(name),
                res = Result(fnTy),
              )

              // Keep the function value definition before any potential nested uses
              // (hierarchical dominance against uses inside lifted function bodies).
              // RewriteMethods.insertOpsAt(InsertPoint.atStartOf(top), cst)
              RewriteMethods.insertOpsAfter(fn, cst)

              // Replace all uses of the lambda value with the constant value
              // (upcast to Attribute to match helper signature)
              RewriteMethods.replaceValue(vl.res, cst.res)

              // Erase the original VLambda
              RewriteMethods.eraseOp(vl)

            case _ => op.regions.foreach(walkRegion)
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
          val funV: Value[Attribute] = app.fun // widen for runtime inspection
          val ft: FunctionType =
            funV.typ match
              case f: FunctionType => f
              case other           =>
                throw new Exception(
                  s"lower-tlam-to-func: expected callee of call_indirect to have builtin.function_type, got $other"
                )

          CallIndirect(
            callee = funV.asInstanceOf[Operand[FunctionType]],
            callee_operands = Seq(app.arg), // arguments only
            _results = ft.outputs
              .map(Result(_)), // result types from function type
          )
        },
        pattern { case vr: VReturn =>
          Return(Seq(vr.value))
        },
      )
    )

    PatternRewriteWalker(p).rewrite(m)
