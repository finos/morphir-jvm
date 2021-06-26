package org.finos.morphir.ir
import org.finos.morphir.ir.*

final case class Type[+A](attributes:A, typeDetails:TypeDetails[A]):
    self =>
    def mapAttributes[B](f: A => B):Type[B] = 
        Type(
            attributes = f(self.attributes), 
            typeDetails = typeDetails.mapAttributes(f))

object Type {
    val unit:Type[scala.Unit] = Type((), TypeDetails.Unit)
    def unit[A](attributes: A):Type[A] = Type(attributes, TypeDetails.Unit)
}

enum TypeDetails[+A]: 
    self =>

    case Variable(name:Name)
    case Reference(typeName:FQName, typeParameters:List[Type[A]])
    case Tuple(elementTypes:List[Type[A]])
    case Record(fields:List[Field[A]])
    case ExtensibleRecord(name:Name, fields:List[Field[A]])
    case Function(parameters:Type[A], returnType:Type[A])
    case Unit

    def mapAttributes[B](f:A=>B):TypeDetails[B] = self match
        case Variable(name) => Variable(name)
        case Reference(typeName, typeParameters) => 
            Reference(typeName, typeParameters.mapAttributes(f))
        case Tuple(elementTypes) => 
            Tuple(elementTypes.mapAttributes(f))
        case Record(fields) => 
            Record(FieldList.mapAttributes(fields)(f))
        case ExtensibleRecord(name:Name, fields) => 
            ExtensibleRecord(name, FieldList.mapAttributes(fields)(f))
        case Function(parameters, returnType) =>
            Function(
                parameters = parameters.mapAttributes(f), 
                returnType = returnType.mapAttributes(f))
        case Unit => Unit

extension [A] (self:List[Type[A]])
    def mapAttributes[B](f:A=>B):List[Type[B]] =
        self.map(t => t.mapAttributes(f))

