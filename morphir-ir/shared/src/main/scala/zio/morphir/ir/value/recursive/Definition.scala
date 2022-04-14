package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.ir.Type.{Type, UType}
import zio.morphir.ir.value.Pattern.{AsPattern, WildcardPattern}
import zio.morphir.ir.value.Specification
import zio.morphir.ir.{InferredTypeOf, Literal, Name}
import zio.prelude._

final case class Definition[+TA, +VA](
    inputTypes: Chunk[(Name, VA, Type[TA])],
    outputType: Type[TA],
    body: Value[TA, VA]
) { self =>

  import Definition._
  import Value.Lambda

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

  def toCase: Case[TA, VA, Type, Value[TA, VA]] = Case(self.inputTypes, self.outputType, self.body)

  def toSpecification: Specification[TA] =
    Specification(
      inputTypes.map { case (n, _, t) => (n, t) },
      output = self.outputType
    )

}

object Definition {
  def apply[TA, VA](outputType: Type[TA], body: Value[TA, VA]): Definition[TA, VA] =
    Definition(Chunk.empty, outputType, body)

  def apply[TA, VA](
      inputTypes: (String, VA, Type[TA])*
  )(outputType: Type[TA])(body: Value[TA, VA]): Definition[TA, VA] = {
    val args = Chunk.fromIterable(inputTypes.map { case (n, va, t) => (Name.fromString(n), va, t) })
    Definition(args, outputType, body)
  }

  def fromLiteral[VA, T](attributes: VA, literal: Literal[T])(implicit
      inferredType: InferredTypeOf[Literal[T]]
  ): Definition[Any, VA] =
    Definition(
      inputTypes = Chunk.empty,
      outputType = literal.inferredType,
      body = Value.Literal(attributes, literal)
    )

  def fromLiteral[T](literal: Literal[T])(implicit inferredType: InferredTypeOf[Literal[T]]): Definition.Typed =
    Definition(
      inputTypes = Chunk.empty,
      outputType = literal.inferredType,
      body = Value.Literal(inferredType.inferredType(literal), literal)
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
  final case class Case[+TA, +VA, +TypeRepr[+_]: Covariant, +Z](
      inputTypes: Chunk[(Name, VA, TypeRepr[TA])],
      outputType: TypeRepr[TA],
      body: Z
  ) { self =>
    def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Case[TB, VB, TypeRepr, Z] =
      Case(
        inputTypes.map { case (n, va, t) => (n, g(va), t.map(f)) },
        outputType.map(f),
        body
      )

    def map[Z2](f: Z => Z2): Case[TA, VA, TypeRepr, Z2] =
      Case(inputTypes, outputType, f(body))
  }

  object Case {
    implicit class CaseExtension[+TA, +VA](private val self: Case[TA, VA, Type, Value[TA, VA]]) extends AnyVal {
      def toDefinition: Definition[TA, VA] = Definition(self.inputTypes, self.outputType, self.body)
    }
  }

  type Raw = Definition[Any, Any]
  object Raw {

    def apply(inputTypes: Chunk[(Name, UType)], outputType: UType, body: RawValue): Raw =
      Definition(inputTypes.map { case (n, t) => (n, (), t) }, outputType, body)

    def apply(outputType: UType, body: RawValue): Raw =
      Definition(
        inputTypes = Chunk.empty,
        outputType = outputType,
        body = body
      )

    def apply(inputTypes: (String, UType)*)(outputType: UType)(body: RawValue): Raw = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), (), t) })
      Definition(args, outputType, body)
    }
  }

  type Typed = Definition[Any, UType]
  object Typed {
    def apply(inputTypes: (String, UType)*)(outputType: UType)(body: TypedValue): Typed = {
      val args = Chunk.fromIterable(inputTypes.map { case (n, t) => (Name.fromString(n), t, t) })
      Definition(args, outputType, body)
    }
  }
}
