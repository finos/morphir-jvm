package morphir.lang.scala.naming

import morphir.ir.FQName
import zio._

import scala.meta.{ Term, Type => MType }

object Naming {
  trait Service {
    def toTypeRef(fullyQualifiedName: FQName): scala.meta.Type.Ref
  }

  val live = ZLayer.succeed(new Service {
    def toTypeRef(fullyQualifiedName: FQName): MType.Ref = {
      val name = MType.Name(fullyQualifiedName.localName.toTitleCase)

      val parts =
        fullyQualifiedName.packagePath.mapSegments(name => Term.Name(name.toLowerCase)) ++ fullyQualifiedName.modulePath
          .mapSegments(name => Term.Name(name.toLowerCase))

      parts.foldLeft[MType.Ref](name) {
        case (localName: MType.Name, qual: Term.Name) => MType.Select(qual, localName)
        case (MType.Select(ref, localName), qual)     => MType.Select(Term.Select(ref, qual), localName)
      }

    }
  })
}
