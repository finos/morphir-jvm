package zio.morphir.lang.metaprog.parsing
import scala.quoted._
import zio.morphir.ir.Module
import zio.morphir.ir.Type.UType
import zio.morphir.lang.util.Format
import zio.morphir.lang.metaprog.parsing.Extractors._
import zio.morphir.lang.util.debug.PrintMacro

object ModuleParser {
  type ModuleDef = Module.Definition[Unit, UType]
  inline def ModuleDef = Module.Definition
  type ModuleSpec = Module.Specification[Unit]
  inline def ModuleSpec = Module.Specification

  final case class ModuleInfo[A](module: A, spec: ModuleSpec, moduleDef: ModuleDef)

  // transparent inline def library[T](inline body: => Tuple):Tuple = body

  inline def apply[T](inline body: T) = ${ parseSpecAndDef('body) }

  inline def module[T](inline body: T) = ${ parseSpecAndDef('body) }

  def error(code: Expr[_])(using Quotes): Nothing =
    import quotes.reflect._
    val msg =
      "======== Could not parse the expression: =======\n" +
        (Format(Printer.TreeAnsiCode.show(code.asTerm))) + "\n" +
        ("================= Matchers =================") + "\n" +
        (Format(Printer.TreeStructure.show(Untype(code.asTerm))))
    report.errorAndAbort(msg)

  object MorphirClassDef:
    def unapply(using Quotes)(term: quotes.reflect.Statement): Option[(String, List[quotes.reflect.Statement])] =
      import quotes.reflect._
      term match
        case ClassDef(
              "$anon",
              DefDef("<init>", List(TermParamClause(Nil)), Inferred(), None),            // Constructor
              List(Apply(Select(New(Inferred()), "<init>"), Nil), TypeIdent(className)), // Constructor Args
              None,                                                                      // Self Type
              methods
            ) =>
          Some((className, methods))
        case _ =>
          None

  object MorphirMethodDef:
    def unapply(using Quotes)(term: quotes.reflect.Statement): Option[
      (
          /*method name*/ String, /*variables*/ List[quotes.reflect.ValDef], /*body*/ quotes.reflect.Term,
          quotes.reflect.TypeTree
      )
    ] =
      import quotes.reflect._
      term match
        case DefDef(
              methodName,
              List(TermParamClause(variables)),
              outputType,
              Some(body)
            ) =>
          Some((methodName, variables, body, outputType))
        case _ =>
          None

  object MorphirMethodVarDef:
    def unapply(using Quotes)(term: quotes.reflect.Tree): Option[(String, quotes.reflect.TypeTree)] =
      import quotes.reflect._
      term match
        case ValDef(varName, varType, None) =>
          Some((varName, varType))
        case _ =>
          None

  def parseSpecAndDef[T: Type](body: Expr[T])(using Quotes): Expr[ModuleInfo[T]] = {
    import quotes.reflect._
    val spec = '{ ModuleSpec.empty }
    val defn = '{ ModuleDef.empty }

    val deser = body.asTerm.underlyingArgument.asExpr
    deser.asTerm match {
      case Block(
            List(
              m @ MorphirClassDef(name, methods) // ClassDef(_, _, _, _, _)
            ),
            _ // Typed(Apply(Select(New(TypeIdent("$anon")), "<init>"), Nil), Inferred())
          ) =>
        // println(s"========== The Whole thing ========\n${Format(Printer.TreeStructure.show(deser.asTerm))}")
        println("=============== Matched Class! ==================")
        println(s"Name: $name")
        println(Printer.TreeStructure.show(m))
        println(s"Name: ${name}")
        println(methods.map(Printer.TreeStructure.show(_)).mkString("\n"))

        methods.map { case MorphirMethodDef(name, vars, body, ret) =>
          println(s"=============== Matched Method ${name}! ==================")
          println(Printer.TreeStructure.show(body))

          vars.map { case MorphirMethodVarDef(name, tpeIdent) =>
            println(s"=============== Matched Method Var ${name}: ${tpeIdent.show}! ==================")
          }
        }

      case _ =>
        // println("============ NOT MATCHED! ==========")
        error(deser)

    }

    '{ ModuleInfo($body, $spec, $defn) }
  }

  // def parseModule[]
}

// object Example {
//   trait Stuff {
//       def add(i:Int, j:Int):Int
//   }

//   val StuffImple = PrintMacro{
//     object StuffImpl extends Stuff {
//         def add(i: Int, j: Int) = i+j //
//     }
//     StuffImpl
//   }

//   import ModuleParser.module
//   val StuffImpl2 = module {
//     object StuffImpl extends Stuff {
//         def add(i: Int, j: Int) = i+j //
//     }
//   }
// }

// val packageSpec = {
//     val c1ModuleSpec = ModuleParser.parseSpec[ModuleContract] {
//         new ModuleContract
//     }
//     val c2ModuleSpec = ModuleParser.parseSpec[ModuleContract] {
//         new ModuleContract
//     }
//     (c1ModuleSpec, c2ModuleSpec)

// }

// parseModule {
//     object Module {
//         def method = ....
//     }
// }

// trait ModuleContract {
//     def method(value:Int):Int
// }

// trait Module2Contract {

// }

// object ModuleContractV1 extends ModuleContract {
//     inline def method(inline value:Int) = value + 1
// }

// val (module, morphirModule) =
//     parseModule {
//         new ModuleContract {
//             def method(value:Int) = ModuleContractV1.method(value)
//         }
//     }
// val (module, morphirModule) =
//     parseModule {
//         new ModuleContract {
//             def method(value:Int) = value + 1
//         }
//     }
