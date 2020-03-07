package morphir.ir.advanced
import morphir.ir.{FQName, Name}

sealed abstract class Value[+X](extra: X)

object Value {
  type Lit = morphir.ir.advanced.Literal

  case class Literal[X](value: Lit, extra: X) extends Value[X](extra)
  case class Constructor[X](fullyQualifiedName: FQName, extra: X)
      extends Value[X](extra)

  case class Tuple[X](elements: List[Value[X]], extra: X)
      extends Value[X](extra)

  case class List[X](items: List[Value[X]], extra: X) extends Value[X](extra)

  case class Record[X](fields: List[Value[X]], extra: X) extends Value[X](extra)

  case class Variable[X](name: Name, extra: X) extends Value[X](extra)
  case class Reference[X](fullyQualifiedName: FQName, extra: X)
      extends Value[X](extra)

  case class Field[X](subjectValue: Value[X], fieldName: Name, extra: X)
      extends Value[X](extra)

  case class FieldFunction[X](fieldName: Name, extra: X) extends Value[X](extra)

  case class Apply[X](function: Value[X], argument: Value[X], extra: X)
      extends Value[X](extra)

  case class Lambda[X](argumentPattern: Pattern[X], body: Value[X], extra: X)
      extends Value[X](extra)

  case class LetDef[X](
      valueName: Name,
      valueDefinition: Definition[X],
      inValue: Value[X],
      extra: X
  ) extends Value[X](extra)

  case class LetRec[X](
      valueDefinitions: List[(Name, Definition[X])],
      inValue: Value[X],
      extra: X
  ) extends Value[X](extra)

  case class LetDestruct[X](
      pattern: Pattern[X],
      valueToDestruct: Pattern[X],
      inValue: Pattern[X],
      extra: X
  ) extends Value[X](extra)

  case class IfThenElse[X](
      condition: Value[X],
      thenBranch: Value[X],
      elseBranch: Value[X],
      extra: X
  ) extends Value[X](extra)

  case class PatternMatch[X](
      branchOutOn: Value[X],
      cases: PatternMatchCasesList[X],
      extra: X
  ) extends Value[X](extra)
  case class Update[X](
      valueToUpdate: Value[X],
      fieldsToUpdate: UpdateFieldsList[X],
      extra: X
  ) extends Value[X](extra)
  case class Unit[X](extra: X) extends Value[X](extra)
}

sealed abstract class Literal {
  def isBoolLiteral: Boolean = false
  def isCharLiteral: Boolean = false
  def isStringLiteral: Boolean = false
  def isIntLiteral: Boolean = false
  def isFloatLiteral: Boolean = false
}

object Literal {
  case class BoolLiteral(value: Boolean) extends Literal {
    override def isBoolLiteral: Boolean = true
  }
  case class CharLiteral(value: Char) extends Literal {
    override def isCharLiteral: Boolean = true
  }
  case class StringLiteral(value: String) extends Literal {
    override def isStringLiteral: Boolean = true
  }
  case class IntLiteral(value: Int) extends Literal {
    override def isIntLiteral: Boolean = true
  }
  case class FloatLiteral(value: Float) extends Literal {
    override def isFloatLiteral: Boolean = true
  }
}

sealed abstract class Pattern[+X](extra: X)
object Pattern {
  case class WildcardPattern[X](extra: X) extends Pattern[X](extra)
  case class AsPattern[X](pattern: Pattern[X], name: Name, extra: X)
      extends Pattern[X](extra)
  case class TuplePattern[X](elementPatterns: List[Pattern[X]], extra: X)
      extends Pattern[X](extra)
  case class RecordPattern[X](fieldNames: List[Name], extra: X)
      extends Pattern[X](extra)

  case class ConstructorPattern[X](
      constructorName: FQName,
      argumentPatterns: List[Pattern[X]],
      extra: X
  ) extends Pattern[X](extra)

  case class EmptyListPattern[X](extra: X) extends Pattern[X](extra)

  case class HeadTailPattern[X](
      headPattern: Pattern[X],
      tailPattern: Pattern[X],
      extra: X
  ) extends Pattern[X](extra)

  case class LiteralPatterns[X](value: Literal, extra: X)
      extends Pattern[X](extra)
}
