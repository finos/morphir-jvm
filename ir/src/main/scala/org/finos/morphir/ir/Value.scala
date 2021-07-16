package org.finos.morphir.ir
import scala.collection.immutable.ListMap

type RawValue = Value[Unit, Unit]
val RawValue = Value

final case class Value[+TA, +VA](ann: VA, valueDetails: ValueDetails[VA, TA])

enum ValueDetails[+TA, +VA]:
  case Literal(literal: Lit)
  case Constructor(fqName: FQName)
  case Tuple(components: scala.List[Value[TA, VA]])
  case List(elements: scala.List[Value[TA, VA]])
  case Record(fields: scala.List[(Name, Value[TA, VA])])
  case Variable(name: Name)
  case Reference(fqName: FQName)
  case Field(target: Value[TA, VA], name: Name)
  case FieldFunction(name: Name)
  case Apply(function: Value[TA, VA], parameters: Value[TA, VA])
  case Lambda(parameters: Pattern[VA], body: Value[TA, VA])
  case LetDefinition(
      binding: Name,
      definition: ValueDefinition[TA, VA],
      inValue: Value[TA, VA]
    )
  case LetRecursion(definitions: ListMap[Name, ValueDefinition[TA, VA]], inValue: Value[TA, VA])
  case Destructure(
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
    )
  case IfThenElse(
      condition: Value[TA, VA],
      thenBranch: Value[TA, VA],
      elseBranch: Value[TA, VA]
    )
  case PatternMatch(matchOn: Value[TA, VA], cases: scala.List[(Pattern[VA], Value[TA, VA])])
  case UpdateRecord(valueToUpdate: Value[TA, VA], fieldsToUpdate: Value[TA, VA])
  case Unit
