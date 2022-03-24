package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.Pattern.{AsPattern, WildcardPattern}
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.value.Value.Lambda

final case class Definition[+TA, +VA](
    inputTypes: Chunk[(Name, VA, Type[TA])],
    outputType: Type[TA],
    body: Value[TA, VA]
) { self =>

  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Definition[TB, VB] =
    Definition(
      inputTypes.map { case (n, va, t) => (n, g(va), t.mapAttributes(f)) },
      outputType.mapAttributes(f),
      body.mapAttributes(f, g)
    )

  def toValue: Value[TA, VA] = self.inputTypes.toList match {
    case Nil => self.body
    case (firstArgName, va, _) :: restOfArgs =>
      val definition = self.copy(inputTypes = Chunk.fromIterable(restOfArgs))
      Lambda(
        attributes = va,
        argumentPattern = AsPattern(WildcardPattern(va), firstArgName, va),
        body = definition.toValue
      )
  }

  def toSpecification: Specification[TA] = {
    Specification(
      inputTypes.map { case (n, _, t) => (n, t) },
      output = self.outputType
    )
  }

}
