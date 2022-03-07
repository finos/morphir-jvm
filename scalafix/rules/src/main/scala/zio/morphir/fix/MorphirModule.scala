package zio.morphir.fix

import scala.meta.*
import scalafix.lint.Diagnostic
import scalafix.Patch
import zio.morphir.ir.{Name => MName}

final case class MorphirModule(
    name: String,
    namespace: String,
    packageName: String,
    body: List[Tree]
)

object MorphirModule {
  def find(source: Tree): List[Result[MorphirModule]] = {
    source.collect {
      case PackageAndBody(pkg, body) =>
        println(s"package: $pkg")
        body.foreach { tree =>
          println("================================================================================")
          println(s"pkg: $pkg")
          println("--------------------------------------------------------------------------------")
          println(s"body: ${tree.syntax}")
          println("--------------------------------------------------------------------------------")
          println(s"body(tree): ${tree.structure}")
          println("================================================================================")
          println("********************************************************************************")
        }

        List(Result.succeed(MorphirModule(pkg.mkString("."), pkg.mkString("."), "DistPackage", body)))
      case _ => Nil
    }.flatten
  }

  // def find(source: Tree): List[Result[MorphirModule]] = {
  //   source.collect { case defn @ Defn.Object(mods, name, _) =>
  //     Result.succeed(MorphirModule(name.value, "", "", defn))
  //   }
  // }

  def toPatch(source: Tree)(f: MorphirModule => Patch): Patch = Patch.fromIterable(find(source).map {
    case Result.Success(w, module) => f(module)
    case Result.Failure(w, errors) => Patch.fromIterable(errors.map(Patch.lint))
  })

  object PackageAndBody {
    private object PackageParts {
      def unapply(tree: Tree): Option[List[String]] =
        tree match {
          case Term.Select(base, Term.Name(last)) => unapply(base).map(last :: _)
          case Term.Name(p)                       => Some(List(p))
          case _                                  => None
        }
    }

    def unapply(tree: Tree): Option[(List[String], List[Tree])] = tree match {
      case Pkg(Term.Name(name), List(pkg @ Pkg(_, _))) =>
        unapply(pkg).map { case (inner, body) =>
          (name :: inner, body)
        }
      case Pkg(PackageParts(pkg), body) => Some((pkg.reverse, body))
      case _                            => None
    }
  }

  object TopLevelType {
    final case class MatchData(packageSegments: List[String], name: String)

    
  }

  object TraitOrClass {
    def unapply(
        tree: Tree
    ): Option[(Either[Defn.Class, Defn.Trait], List[Mod], Type.Name, List[Type.Param], Template)] = tree match {
      case defn @ Defn.Trait(mods, name, tparams, _, template) => Some((Right(defn), mods, name, tparams, template))
      case defn @ Defn.Class(mods, name, tparams, _, template) => Some((Left(defn), mods, name, tparams, template))
      case _                                                   => None
    }
  }

  // object Object {
  //   def unapply(tree: Tree): Option[(Defn.Object, List[Mod], Type.Name, List[Type.Param], Template)] = tree match {
  //     case defn @ Defn.Object(mods, name, tparams, _, template) => Some((defn, mods, name, tparams, template))
  //     case _                                                    => None
  //   }
  // }
}
