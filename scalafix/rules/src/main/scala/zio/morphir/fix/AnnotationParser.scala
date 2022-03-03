package zio.morphir.fix
import scala.meta.*

object AnnotationParser {
  object ModuleAnnotation extends AnnotationPattern("module")

  private def isAnnotationNamed(name: String)(typeTree: Type): Boolean = typeTree match {
    case Type.Select(_, Type.Name(`name`)) => true
    case Type.Name(`name`)                 => true
    case _                                 => false
  }
  class AnnotationPattern(name: String) {
    def unapply(mods: List[Mod]): Option[List[Term]] = mods.reverse.collectFirst {
      case Mod.Annot(Init(typeTree, Name(""), args)) if isAnnotationNamed(name)(typeTree) => Some(args.flatten)
    }.flatten
  }
}
