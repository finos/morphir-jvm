package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.types.recursive.Definition
import zio.morphir.ir.{Documented, Name}

sealed trait Specification[+Attributes] { self =>
  import Specification._

  def ??(doc: String): Documented[Specification[Attributes]] =
    Documented(doc, self)

  def map[B](f: Attributes => B): Specification[B] = self match {
    case TypeAliasSpecification(typeParams, expr)   => TypeAliasSpecification(typeParams, expr.map(f))
    case spec @ OpaqueTypeSpecification(_)          => spec
    case CustomTypeSpecification(typeParams, ctors) => CustomTypeSpecification(typeParams, ctors.map(f))
  }

  def eraseAttributes: Specification[Any] = self match {
    case c @ TypeAliasSpecification(_, _) =>
      TypeAliasSpecification(c.typeParams, c.expr.eraseAttributes)
    case c @ OpaqueTypeSpecification(_) =>
      OpaqueTypeSpecification(c.typeParams)
    case c @ CustomTypeSpecification(_, _) =>
      CustomTypeSpecification(c.typeParams, c.ctors.eraseAttributes)
  }
}

private[ir] object Specification {

  def mapSpecificationAttributes[A](spec: Specification[A]): MapSpecificationAttributes[A] =
    new MapSpecificationAttributes(() => spec)

  final case class TypeAliasSpecification[+Attributes](
      typeParams: Chunk[Name],
      expr: Type[Attributes]
  ) extends Specification[Attributes]

  final case class OpaqueTypeSpecification(typeParams: Chunk[Name]) extends Specification[Nothing]

  object OpaqueTypeSpecification {
    def apply(typeParams: String*): OpaqueTypeSpecification =
      OpaqueTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)))
  }

  final case class CustomTypeSpecification[+Attributes](
      typeParams: Chunk[Name],
      ctors: Constructors[Attributes]
  ) extends Specification[Attributes]

  object CustomTypeSpecification {
    def fromCtors[Attributes](
        ctor: (String, Iterable[(String, Type[Attributes])]),
        ctors: (String, Iterable[(String, Type[Attributes])])*
    ): CustomTypeSpecification[Attributes] = {
      val allCtors = (ctor +: ctors).map { case (name, args) =>
        (
          Name.fromString(name),
          Chunk.fromIterable(args.map { case (name, tpe) =>
            (Name.fromString(name), tpe)
          })
        )
      }.toMap
      CustomTypeSpecification(Chunk.empty, Constructors(allCtors))
    }

    def mkEnum(case1: String, otherCases: String*): CustomTypeSpecification[Any] =
      CustomTypeSpecification(Chunk.empty, Constructors.forEnum(case1, otherCases: _*))
  }

  type UCustomTypeSpecification = CustomTypeSpecification[Any]
  val UCustomTypeSpecification: CustomTypeSpecification.type = CustomTypeSpecification

  type USpecification = Specification[Any]
  val USpecification: Specification.type = Specification

  final class MapSpecificationAttributes[+A](val input: () => Specification[A]) extends AnyVal {
    def map[B](f: A => B): Specification[B] = input().map(f)
  }

  def fromDefinition[A](definition: Definition[A]): Specification[A] = definition match {
    case Definition.TypeAlias(typeParams, typeExp) => TypeAliasSpecification(typeParams, typeExp)
    case Definition.CustomType(typeParams, acessCtors) =>
      acessCtors.withPublicAccess match {
        case Some(ctors) => CustomTypeSpecification(typeParams, ctors)
        case None        => OpaqueTypeSpecification(typeParams)
      }
  }
}
