package zio.morphir.lang.util.debug

import scala.quoted._
import zio.morphir.lang.util.Format
import zio.morphir.lang.metaprog.parsing.Extractors._
//import io.getquill.metaprog.DeserializeAstInstances

object PrintMacro {
    inline def apply(inline any: Any, inline deserializeAst: Boolean = false): Unit = ${ printMacImpl('any, 'deserializeAst) }
    def printMacImpl(anyRaw: Expr[Any], deserializeAstRaw: Expr[Boolean])(using Quotes): Expr[Unit] = {
      import quotes.reflect._

      val deserializeAst =
        deserializeAstRaw match
          case '{ true } => true
          case '{ false } => false
          case _ => report.errorAndAbort("deserializeAst must be a constant value true/false")

      val any = anyRaw.asTerm.underlyingArgument.asExpr
      val deser =
        if (deserializeAst)
          //DeserializeAstInstances(any)
          any
        else
          any

      println("================= IN PRINT MACRO =================")
      println("================= Tree =================")
      println(Format(Printer.TreeAnsiCode.show(deser.asTerm)))

      println("================= Matchers =================")
      println(Format(Printer.TreeStructure.show(Untype(deser.asTerm))))

      //println("================= Pretty Tree =================")
      //println(pprint.apply(Untype(any.asTerm)))

      '{ () }
    }
}