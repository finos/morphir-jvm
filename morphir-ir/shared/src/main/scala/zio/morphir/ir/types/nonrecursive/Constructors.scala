package zio.morphir.ir.types.nonrecursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name}

private[ir] final case class Constructors[+Attributes](toMap: Map[Name, Chunk[(Name, Type[Attributes])]])
    extends AnyVal { self =>
  def eraseAttributes: Constructors[Any] = Constructors(toMap.map { case (ctor, args) =>
    (ctor, args.map { case (paramName, paramType) => (paramName, paramType.eraseAttributes) })
  })

  def map[B](f: Attributes => B): Constructors[B] =
    Constructors(toMap.map { case (name, ctors) =>
      (name, ctors.map { case (name, tpe) => (name, tpe.map(f)) })
    })

  def collectReferences: Set[FQName] =
    toMap.values.flatMap {
      case Chunk((_, tpe)) =>
        tpe.collectReferences
      case _ => Nil
    }.toSet

  def ctorNames: Set[Name] = toMap.keySet
}

private[ir] object Constructors {

  def forEnum(case1: String, otherCases: String*): Constructors[Any] = {
    val allCases  = (Chunk(case1) ++ otherCases).map(Name.fromString)
    val emptyArgs = Chunk[(Name, UType)]()
    Constructors(allCases.map(name => (name, emptyArgs)).toMap)
  }

}
