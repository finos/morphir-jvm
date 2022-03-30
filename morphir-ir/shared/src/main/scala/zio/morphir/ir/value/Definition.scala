package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.Pattern.{AsPattern, WildcardPattern}
import zio.morphir.ir.types.Type
import zio.morphir.ir.value.Value.Lambda

final case class Definition[+TA, +VA](
    inputTypes: Chunk[(Name, VA, Type[TA])],
    outputType: Type[TA],
    body: Value[TA, VA]
) { self =>

  import Definition._

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

  def toCase: Case[TA, VA, Value[TA, VA]] = Case(self.inputTypes, self.outputType, self.body)

  def toSpecification: Specification[TA] = {
    Specification(
      inputTypes.map { case (n, _, t) => (n, t) },
      output = self.outputType
    )
  }

}

object Definition {
  final case class Case[+TA, +VA, +Z](
      inputTypes: Chunk[(Name, VA, Type[TA])],
      outputType: Type[TA],
      body: Z
  ) { self =>
    def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Case[TB, VB, Z] =
      Case(
        inputTypes.map { case (n, va, t) => (n, g(va), t.mapAttributes(f)) },
        outputType.mapAttributes(f),
        body
      )

    def map[Z2](f: Z => Z2): Case[TA, VA, Z2] =
      Case(inputTypes, outputType, f(body))
  }

  object Case {
    implicit class CaseExtension[+TA, +VA](val self: Case[TA, VA, Value[TA, VA]]) extends AnyVal {
      def toDefinition: Definition[TA, VA] = Definition(self.inputTypes, self.outputType, self.body)
    }
  }
}
