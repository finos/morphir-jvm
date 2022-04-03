package zio.morphir.ir.module
import zio.morphir.ir.{Documented, Name}
import zio.morphir.ir.types.{Specification => TypeSpecification}
import zio.morphir.ir.value.{Specification => ValueSpecification}

final case class Specification[+TA](
    types: Map[Name, Documented[TypeSpecification[TA]]],
    values: Map[Name, Documented[ValueSpecification[TA]]]
) {
  def lookupValue(localName: Name): Option[ValueSpecification[TA]] =
    values.get(localName).map(_.value)
  def lookupType(localName: Name): Option[zio.morphir.ir.types.Specification[TA]] =
    types.get(localName).map(doc => doc.value)

  def eraseAttributes: Specification[TA] = Specification.empty

  def mapAttributes: Specification[TA] = ???
}

object Specification {
  val empty: Specification[Nothing] = Specification(Map.empty, Map.empty)

  type Raw = Specification[Unit]
  object Raw {
    def apply(
        types: Map[Name, Documented[TypeSpecification[Unit]]],
        values: Map[Name, Documented[ValueSpecification[Unit]]]
    ): Raw = Specification(types, values)
  }
}
