package morphir.lang.scala
import morphir.ir.FQName

import scala.meta.{ Term, Type => MType }

object Naming {
  implicit class FQNameOps(private val fqn: FQName) extends AnyVal {
    def toTypeName: scala.meta.Type.Name =
      scala.meta.Type.Name(fqn.toString)

    def toTypeRef: MType.Ref = {
      val name = MType.Name(fqn.localName.toTitleCase)

      val parts =
        fqn.packagePath.mapSegments(name => Term.Name(name.toLowerCase)) ++ fqn.modulePath
          .mapSegments(name => Term.Name(name.toLowerCase))

      parts.foldLeft[MType.Ref](name) {
        case (localName: MType.Name, qual: Term.Name) => MType.Select(qual, localName)
        case (MType.Select(ref, localName), qual)     => MType.Select(Term.Select(ref, qual), localName)
      }

    }
  }
}
