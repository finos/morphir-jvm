package zio.morphir.lang.example
import scala.annotation.StaticAnnotation
import zio.morphir.lang.util.debug.PrintMacro
import zio.morphir.lang.metaprog.parsing.ModuleParser
import ModuleParser.{apply => _, _}

//class name(value: String) extends StaticAnnotation

trait Stuff {
  def add(a: Int, b: Int): Int
}
object Main {
  def main(args: Array[String]): Unit = module { //
    new Stuff {
      def add(a: Int, b: Int): Int = a + b
    }
  }
}

//
//
// trait BasicModule[Amount](input: Int) {
//   val basicModuleValue = input
//   def add(l: Amount, r: Amount): Amount
// }

// trait Stuff[T] {
//   final type Type = T;
//   def add(i:Type, j:Type):Type
// }
// // object ModuleDefinitionExample {
// //   def main(args: Array[String]): Unit = {
// //     PrintMacro {
// //       object Basic2 extends BasicModule[Int](123) { selfPointer =>
// //         val v                        = 123
// //         def add(l: Int, r: Int): Int = l + r
// //       }
// //       Basic2
// //     }

// //   }

// // }

// object Example {
//   trait StuffUsage[T] {
//     def calculate(seed:T):T
//   }
//   inline def stuffModule: ModuleInfo[Stuff[Int]] = module {
//     new Stuff[Int] {
//       def add(i: Int, j: Int):Int = i+j ////
//     }
//   }

//   inline def usageStuff(inline stuffInfo:ModuleInfo[Stuff[Int]]) = module {
//     val module = stuffInfo.module
//     import module._
//     new StuffUsage[Int] {
//       def calculate(seed:Int):Int = add(1,2) + seed
//     }
//   }

//   inline def myLib = library {
//     (
//        stuffModule.module,
//        usageStuff(stuffModule).module
//     )
//   }

//   PrintMacro(myLib)

// class Parser(info: ModuleInfo[_]) {
//   def parse =
//     case '{ (value: Stuff[Int]).add($a, $b) } =>
//       info.lookup("Stuff.add").call(parse(a), parse(b))
// }

// val BadStuffModule = module {
//   new Stuff {
//     type T = Int
//     def add(i: Int, j: Int) = {
//       val bad : String = 5;
//       i+j ////
//     }
//   }
// }

// object NormalStuffModule extends Stuff[Int] {
//     def add(i: Int, j: Int) = i+j ////
// }

// def main(args: Array[String]): Unit = {

//   pprint.pprintln(myLib)

//   // PrintMacro {
//   //   stuffModule.module.add(1,2)
//   //   usageStuff(stuffModule.module).module.calculate(1)
//   // }

//   stuffModule.module.add(1,2)
//   val result = usageStuff(stuffModule).module.calculate(1)
//   pprint.pprintln(result)
// }

//val x = stuffModule.module.add(1,2)
//val y : NormalStuffModule.Type = 4;
//SomeMacro[StuffModule]
//SomeMacro[NormalStuffModule]
// }

// case class ModuleSpecDef[T](module: T, spec, def)
// val moduleSpecDef: ModuleSpecDef[BasicModule[Int]] = ModuleParser[BasicModule[Int]] {
//   object Basic2 extends BasicModule[Int](123) { selfPointer =>
//     val v                        = 123
//     def add(l: Int, r: Int): Int = l + r
//   }
//   Basic2
// }
// val foo = moduleSpecDef.module.add(1,1)

//   val moduleSpecDef2: ModuleSpecDef[BasicModule2] = ModuleParser[BasicModule2] {
//     object BasicModule2Impl extends BasicModule2 {
//                                                // Apply(Select(Select(Select(moduleSpecDef), module), add), List(Ident(a), Ident(b)))
//       def addAndMult(a: Int, b: Int, c: Int) = moduleSpecDef.module.add(a, b) * c
//     }
//     BasicModule2Impl
//   }

//   moduleSpecDef2.module.add()

// }

// trait BasicModule2 {
//   def addAndMult(a: Int, b: Int, c: Int): Int
// }

// trait AmountModule {
//   type Amount
//   def add(l: Amount, r: Amount): Amount
//   def eq(l:Amount, r:Amount):Boolean
// }

// object AmountModuleImpl

// trait BoolModule {
//     def and(l: Bool, r: Bool): Bool
// }

// trait Instrument[Amount, Bool](amountModule:AmountModule[Amount],) {
//   final val Amount
// }

/*

case '{ ($amt: Amount).method } => sdk.lookupImplementation("method")


inline def moduleSpecDef: ModuleSpecDeff =
  quote {
    def condition(b: Boolean)
  }

inline def moduleSpecDef: ModuleSpecDeff =
  quote {
    object Rule {
      def condition(b: Boolean)
    }
  }


val myInstrument = instrument {
  ruleset {
    rule {
      condition(foo = bar)
      action(something :+ blah)
    }
  }
}



serializeToFile(module)


 */

/*
================= Matchers =================
      /** Create a class definition tree
 *
 *  @param cls The class symbol. A new class symbol can be created using `Symbol.newClass`.
 *  @param parents The parents trees class. The trees must align with the parent types of `cls`.
 *                 Parents can be `TypeTree`s if they don't have term parameter,
 *                 otherwise the can be `Term` containing the `New` applied to the parameters of the extended class.
 *  @param body List of members of the class. The members must align with the members of `cls`.
 */
      @experimental def apply(cls: Symbol, parents: List[Tree /* Term | TypeTree */], body: List[Statement]): ClassDef

def unapply(cdef: ClassDef): (String, DefDef, List[Tree /* Term | TypeTree */], Option[ValDef], List[Statement])

Block(
  List(
    ClassDef(
      "$anon",
      DefDef("<init>", List(TermParamClause(Nil)), Inferred(), None),
      List(
        Apply(Select(New(Inferred()), "<init>"), Nil),
        TypeIdent("BasicModule")
      ),
      None,
      List(
        DefDef(
          "add",
          List(
            TermParamClause(
              List(
                ValDef("l", TypeIdent("Int"), None),
                ValDef("r", TypeIdent("Int"), None)
              )
            )
          ),
          TypeIdent("Int"),
          Some(Apply(Select(Ident("l"), "+"), List(Ident("r"))))
        )
      )
    )
  ),
  Typed(Apply(Select(New(TypeIdent("$anon")), "<init>"), Nil), Inferred())

 ================= Pretty Tree =================
Block(
  stats = List(
    TypeDef(
      name = $anon,
      rhs = Template(
        constr = DefDef(
          name = <init>,
          paramss = List(List()),
          tpt = TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Unit)],
          preRhs = Thicket(trees = List())
        ),
        parentsOrDerived = List(
          Apply(
            fun = Select(
              qualifier = New(
                tpt = TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class Object)]
              ),
              name = <init>
            ),
            args = List()
          ),
          Apply(
            fun = Select(qualifier = New(tpt = Ident(name = BasicModule)), name = <init>),
            args = List(Literal(const = ( = 1)))
          )
        ),
        self = ValDef(name = _, tpt = Thicket(trees = List()), preRhs = Thicket(trees = List())),
        preBody = List(
          ValDef(
            name = v,
            tpt = TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],
            preRhs = Literal(const = ( = 123))
          ),
          DefDef(
            name = add,
            paramss = List(
              List(
                ValDef(name = l, tpt = Ident(name = Int), preRhs = Thicket(trees = List())),
                ValDef(name = r, tpt = Ident(name = Int), preRhs = Thicket(trees = List()))
              )
            ),
            tpt = Ident(name = Int),
            preRhs = Apply(
              fun = Select(qualifier = Ident(name = l), name = +),
              args = List(Ident(name = r))
            )
          )
        )
      )
    )
  ),
  expr = Typed(
    expr = Apply(
      fun = Select(qualifier = New(tpt = Ident(name = $anon)), name = <init>),
      args = List()
    ),
    tpt = TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class example)),trait BasicModule)]
  )
)
 */
