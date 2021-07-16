package org.finos.morphir.ir
import scala.collection.immutable.ListMap

final case class ModuleSpecification[+A](
    types: ListMap[Name, Documented[TypeSpecification[A]]],
    values: ListMap[Name, ValueSpecification[A]]
  )
