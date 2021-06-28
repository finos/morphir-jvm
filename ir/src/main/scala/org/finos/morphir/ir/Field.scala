package org.finos.morphir.ir

final case class Field[+A](name:String, fieldType:Type[A]):
    def mapAttributes[B](f:A => B):Field[B] = 
        Field(name = name, fieldType = fieldType.transform(f))

object FieldList:
    extension [A] (self:List[Field[A]])
        def mapAttributes[B](f:A=>B):List[Field[B]] =
            self.map(field => field.mapAttributes(f))