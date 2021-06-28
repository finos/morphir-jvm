package org.finos.morphir.ir

final case class Value[+TA,+VA](ann:VA, valueDetails:ValueDetails[VA,TA])

enum ValueDetails[+TA,+VA]:
    case Literal(literal:Lit)
    case Constructor(fqName:FQName)
    case Tuple(components:scala.List[Value[TA,VA]])
    case List(elements:scala.List[Value[TA,VA]])
    case Record(fields:scala.List[(Name, Value[TA,VA])])
    case Variable(name:Name)
    case Reference(fqName:FQName)
    case Field(target:Value[TA,VA], name:Name)
    case FieldFunction(name:Name)
    case Apply(function:Value[TA,VA], parameters:Value[TA,VA])
    case Lambda(parameters:Pattern[VA], body:Value[TA,VA])
    case LetDefinition(binding:Name) //TODO: Finish up
