package org.finos.morphir.ir


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

