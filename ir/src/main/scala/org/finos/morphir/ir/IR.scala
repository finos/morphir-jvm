package org.finos.morphir.ir

case class Name(name:List[String])

case class Path(path:List[Name])

case class FQName(packagePath:Path, modulePath:Path, localName:Name)

case class Type[+A](ann:A, typeDetails:TypeDetails[A])

case class Field[+A](name:String, fieldType:Type[A])
enum TypeDetails[+A]:
    case Variable(name:Name)
    case Reference(fqName:FQName, types:List[Type[A]])
    case Tuple(components:List[Type[A]])
    case Record(fields:List[Field[A]])
    case ExtensibleRecord(name:Name, fields:List[Field[A]])
    case Function(parameters:Type[A], returnType:Type[A])
    case Unit


case class Value[+VA,+TA](ann:VA, valueDetails:ValueDetails[VA,TA])

enum ValueDetails[+VA,+TA]:
    case Literal(literal:Lit)
    case Constructor(fqName:FQName)
    case Tuple(components:scala.List[Value[VA,TA]])
    case List(elements:scala.List[Value[VA,TA]])
    case Record(fields:scala.List[(Name, Value[VA,TA])])
    case Variable(name:Name)
    case Reference(fqName:FQName)
    case Field(target:Value[VA,TA], name:Name)
    case FieldFunction(name:Name)
    case Apply(function:Value[VA,TA], parameters:Value[VA,TA])
    case Lambda(parameters:Pattern[VA], body:Value[VA,TA])

enum Lit:
    case Bool(value:Boolean)
    case Char(value:scala.Char)
    case Str(value:String)
    case Int(value:scala.Int) //TODO: Maybe BigInt
    case Float(value:scala.Double) //TODO: Maybe BigDecimal

case class Pattern[+A](ann:A, details:PatternDetails[A])

enum PatternDetails[+A]:
    case Wildcard
    case As(pattern:Pattern[A], name:Name)
    case Tuple(components:List[Pattern[A]])
    case Constructor(fqName:FQName, parameters:List[Pattern[A]])
    case EmptyList
    case HeadTail(head:Pattern[A], tail:Pattern[A])
    case Literal(literal:Lit)
    case Unit