package org.finos.morphir.ir
import org.finos.morphir.ir.*

final case class Type[+A](annotations: A, typeDetails: TypeDetails[A]):
  def transform[B](f: A => B): Type[B] =
    Type(f(annotations), typeDetails.transform(f))

object Type:
  def reference(fqName: FQName, typeParameters: List[Type[Unit]]): Type[Unit] =
    reference[Unit](fqName, typeParameters)(())

  def reference[A](fqName: FQName, typeParameters: List[Type[A]])(annotations: A): Type[A] =
    Type(annotations, TypeDetails.Reference(fqName, typeParameters))

  val unit: Type[scala.Unit]           = unit(())
  def unit[A](annotations: A): Type[A] = Type(annotations, TypeDetails.Unit)

  /**
   * Creates a type variable.
   */
  def variable(name: Name): Type[Unit] = variable(name)(())
  def variable[A](name: Name)(annotations: A): Type[A] =
    Type(annotations, TypeDetails.Variable(name))

enum TypeDetails[+A]:
  self =>

  case Variable(name: Name)
  case Reference(typeName: FQName, typeParameters: List[Type[A]])
  case Tuple(elementTypes: List[Type[A]])
  case Record(fields: List[Field[A]])
  case ExtensibleRecord(name: Name, fields: List[Field[A]])
  case Function(parameters: Type[A], returnType: Type[A])
  case Unit

  def transform[B](f: A => B): TypeDetails[B] = self match
    case Variable(name) => Variable(name)
    case Reference(typeName, typeParameters) =>
      Reference(typeName, typeParameters.transform(f))
    case Tuple(elementTypes) =>
      Tuple(elementTypes.transform(f))
    case Record(fields) =>
      Record(FieldList.mapAttributes(fields)(f))
    case ExtensibleRecord(name: Name, fields) =>
      ExtensibleRecord(name, FieldList.mapAttributes(fields)(f))
    case Function(parameters, returnType) =>
      Function(parameters = parameters.transform(f), returnType = returnType.transform(f))
    case Unit => Unit

extension [A](self: List[Type[A]])
  def transform[B](f: A => B): List[Type[B]] =
    self.map(t => t.transform(f))
