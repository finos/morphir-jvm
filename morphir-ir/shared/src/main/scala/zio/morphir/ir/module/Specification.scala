package zio.morphir.ir.module

import zio.morphir.ir.Type.{Specification => TypeSpecification}
import zio.morphir.ir.value.{Specification => ValueSpecification}
import zio.morphir.ir.{Documented, Name}

final case class Specification[+TA](
    types: Map[Name, Documented[TypeSpecification[TA]]],
    values: Map[Name, Documented[ValueSpecification[TA]]]
) {
  def lookupValue(localName: Name): Option[ValueSpecification[TA]] =
    values.get(localName).map(_.value)
  def lookupType(localName: Name): Option[TypeSpecification[TA]] =
    types.get(localName).map(doc => doc.value)

  def eraseAttributes: Specification[TA] = Specification.empty

  def mapAttributes: Specification[TA] = ???
}

object Specification {
  val empty: Specification[Nothing] = Specification(Map.empty, Map.empty)

  type Raw = Specification[Any]
  object Raw {
    def apply(
        types: Map[Name, Documented[TypeSpecification[Any]]],
        values: Map[Name, Documented[ValueSpecification[Any]]]
    ): Raw = Specification(types, values)
  }
}
