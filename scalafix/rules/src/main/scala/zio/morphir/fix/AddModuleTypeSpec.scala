package zio.morphir.fix

import scalafix.v1.*
import scala.meta.*
class AddModuleTypeSpec extends SyntacticRule("AddModuleTypeSpec") {
  // override def fix(implicit doc: SyntacticDocument): Patch = MorphirModule.toPatch(doc.tree) { module =>
  //   println(s"module: $module")
  //   Patch.empty
  // }

  override def fix(implicit doc: SyntacticDocument): Patch = {
    MorphirModule
      .toPatch(doc.tree) { module =>
        println(s"Found module: ${module.name}")        
        Patch.empty
      }
    // Patch.empty
    // println("in fix")
    // doc.tree.children.map { child =>
    //   child match {
    //     case Pkg(
    //           _,
    //           statements
    //         ) =>
    //       println(s"Yaaayyy!!!")
    //       statements.collect { case Defn.Object(firstMod :: _, _, _) =>
    //         Patch.addAround(firstMod, "/** Before*/", "/** After*/")
    //       }.asPatch
    //     case unmatched =>
    //       println("unmatched: " + unmatched.syntax)
    //       Patch.empty

    //   }
    // }.asPatch
  }
}
//Pkg(_, List(Import(List(Importer(Term.Select(Term.Name("zio"), Term.Name("morphir")), List(Importee.Name(Name("module")))))), Defn.Object(List(Mod.Annot(Init(Type.Name("module"), Name(""), List(List(Term.Assign(Term.Name("namespace"), Term.Apply(Term.Name("Some"), List(Lit.String("Morphir.SDK"))))))))), Term.Name("Bool"), Template(Nil, Nil, Self(Name(""), None), Nil, Nil))))
