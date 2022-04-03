package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Literal
import zio.morphir.ir.Name
import zio.morphir.ir.types.Type
import zio.morphir.ir.value.Value.Lambda
import Pattern.{AsPattern, WildcardPattern}
import zio.morphir.ir.types.UType
import zio.morphir.ir.InferredTypeOf

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
  def apply[TA, VA](
      inputTypes: (String, VA, Type[TA])*
  )(outputType: Type[TA])(body: Value[TA, VA]): Definition[TA, VA] = {
    val args = Chunk.fromIterable(inputTypes.map { case (n, va, t) => (Name.fromString(n), va, t) })
    Definition(args, outputType, body)
  }

  def fromLiteral[VA, T](attributes: VA, literal: Literal[T])(implicit
      inferredType: InferredTypeOf[Literal[T]]
  ): Definition[Unit, VA] =
    Definition(
      inputTypes = Chunk.empty,
      outputType = literal.inferredType,
      body = Value.Literal(attributes, literal)
    )

  def fromLiteral[T](literal: Literal[T])(implicit inferredType: InferredTypeOf[Literal[T]]): Definition.Typed =
    Definition(
      inputTypes = Chunk.empty,
      outputType = literal.inferredType,
      body = literal.toTypedValue
    )

  def fromRawValue(value: (RawValue, UType)): Definition.Raw =
    Definition(
      inputTypes = Chunk.empty,
      outputType = value._2,
      body = value._1
    )

  def fromRawValue(value: RawValue, outputType: UType): Definition.Raw =
    Definition(
      inputTypes = Chunk.empty,
      outputType = outputType,
      body = value
    )

  def fromTypedValue(value: TypedValue): Definition.Typed =
    Definition(
      inputTypes = Chunk.empty,
      outputType = value.attributes,
      body = value
    )
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

  type Raw = Definition[scala.Unit, scala.Unit]
  object Raw {
    def apply(inputTypes: (String, UType)*)(outputType: UType)(body: RawValue): Raw = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), (), t) })
      Definition(args, outputType, body)
    }
  }

  type Typed = Definition[scala.Unit, UType]
  object Typed {
    def apply(inputTypes: (String, UType)*)(outputType: UType)(body: TypedValue): Typed = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), t, t) })
      Definition(args, outputType, body)
    }
  }
}
