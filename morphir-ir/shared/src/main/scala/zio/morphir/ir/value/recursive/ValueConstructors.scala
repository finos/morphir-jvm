package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.ir.Type.UType
import zio.morphir.ir.sdk.List.listType
import zio.morphir.ir.value.{Pattern, UPattern}
import zio.morphir.ir.{FQName, IsNotAValue, Literal => Lit, Name}

trait ValueConstructors {
  import Value._

  final def apply[TA, VA](
      attributes: VA,
      function: Value[TA, VA],
      argument: Value[TA, VA],
      arguments: Value[TA, VA]*
  ): Value[TA, VA] =
    Apply(attributes, function, argument, arguments: _*)

  final def apply(function: RawValue, argument: RawValue): RawValue = Apply.Raw(function, argument)

  final def apply(function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
    Apply.Typed(function, argument, arguments: _*)

  final def boolean[A](attributes: A, value: Boolean): Value[Nothing, A] = Literal(attributes, Lit.boolean(value))
  final def boolean(value: Boolean): RawValue                            = Literal.Raw(Lit.boolean(value))

  final def emptyTuple[VA](attributes: VA): Value[Nothing, VA] = Tuple(attributes)

  final def caseOf[TA, VA](
      value: Value[TA, VA]
  )(firstCase: (Pattern[VA], Value[TA, VA]), otherCases: (Pattern[VA], Value[TA, VA])*): Value[TA, VA] =
    PatternMatch(value.attributes, value, firstCase +: Chunk.fromIterable(otherCases))

  final def constructor[A](attributes: A, name: String): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor[A](attributes: A, name: FQName): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor(name: String): RawValue                            = Constructor.Raw(name)
  final def constructor(name: FQName): RawValue                            = Constructor.Raw(name)
  final def constructor(name: String, tpe: UType): TypedValue              = Constructor.Typed(name, tpe)
  final def constructor(name: FQName, tpe: UType): TypedValue              = Constructor.Typed(name, tpe)

  final def decimal[A](attributes: A, value: BigDecimal): Value[Nothing, A] = Literal(attributes, Lit.decimal(value))
  final def decimal(value: BigDecimal): RawValue                            = Literal.Raw(Lit.decimal(value))

  final def destructure[TA, VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ): Value[TA, VA] = Destructure(attributes, pattern, valueToDestruct, inValue)

  final def destructure(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: Name): Value[TA, VA] =
    Field(attributes, target, name)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: String): Value[TA, VA] =
    Field(attributes, target, name)

  final def field(target: RawValue, name: Name): RawValue = Field.Raw(target, name)

  final def field(target: RawValue, name: String): RawValue = Field.Raw(target, name)

  final def fieldFunction[A](attributes: A, name: String): Value[Nothing, A] = FieldFunction(attributes, name)
  final def fieldFunction[A](attributes: A, name: Name): Value[Nothing, A]   = FieldFunction(attributes, name)
  final def fieldFunction(name: String, tpe: UType): TypedValue              = FieldFunction.Typed(tpe, name)
  final def fieldFunction(name: Name, tpe: UType): TypedValue                = FieldFunction.Typed(tpe, name)
  final def fieldFunction(name: String): RawValue                            = FieldFunction.Raw(name)
  final def fieldFunction(name: Name): RawValue                              = FieldFunction.Raw(name)

  final def float[A](attributes: A, value: Float): Value[Nothing, A] = Literal(attributes, Lit.float(value))
  final def float(value: Float): RawValue                            = Literal.Raw(Lit.float(value))

  final def ifThenElse[TA, VA](
      attributes: VA,
      condition: Value[TA, VA],
      thenBranch: Value[TA, VA],
      elseBranch: Value[TA, VA]
  ): Value[TA, VA] = IfThenElse(attributes, condition, thenBranch, elseBranch)

  final def ifThenElse(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): RawValue =
    IfThenElse.Raw(condition, thenBranch, elseBranch)

  final def int[A](attributes: A, value: Int): Value[Nothing, A] = Literal(attributes, Lit.int(value))
  final def int(value: Int): RawValue                            = Literal.Raw(Lit.int(value))

  final def lambda[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
    Lambda(attributes, argumentPattern, body)

  final def lambda(argumentPattern: UPattern, body: RawValue): RawValue = Lambda.Raw(argumentPattern, body)

  final def let[TA, VA](
      attributes: VA,
      name: String,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def let[TA, VA](
      attributes: VA,
      name: Name,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def let(name: Name, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def let(name: String, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def let(varName: String, value: Int, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: String, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: Boolean, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: Float, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: Double, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: scala.BigDecimal, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: java.math.BigDecimal, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: scala.BigInt, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def let(varName: String, value: java.math.BigInteger, block: TypedValue): TypedValue = {
    val literalValue = literal(value)
    val vDef         = valueDef(literalValue.attributes).withBody(literalValue)
    LetDefinition.Typed(varName, vDef, block)
  }

  final def letDef[TA, VA](
      attributes: VA,
      name: Name,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def letDef[TA, VA](
      attributes: VA,
      name: String,
      valueDefinition: Definition[TA, VA],
      body: Value[TA, VA]
  ): Value[TA, VA] =
    LetDefinition(attributes, name, valueDefinition, body)

  final def letDef(name: Name, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def letDef(name: String, valueDefinition: Definition.Raw, body: RawValue): RawValue =
    LetDefinition.Raw(name, valueDefinition, body)

  final def letDestruct[TA, VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ): Value[TA, VA] = Destructure(attributes, pattern, valueToDestruct, inValue)

  final def letDestruct(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  final def letRec[TA, VA](
      attributes: VA,
      valueDefinitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA]
  ): Value[TA, VA] =
    LetRecursion(attributes, valueDefinitions, inValue)

  final def letRec[TA, VA](attributes: VA, valueDefinitions: (String, Definition[TA, VA])*)(
      inValue: Value[TA, VA]
  ): Value[TA, VA] =
    LetRecursion(attributes, valueDefinitions: _*)(inValue)

  final def letRec(valueDefinitions: Map[Name, Definition.Raw], inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions, inValue)

  final def letRec(valueDefinitions: (String, Definition.Raw)*)(inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions: _*)(inValue)

  final def list[TA, VA](attributes: VA, values: Chunk[Value[TA, VA]]): Value[TA, VA] =
    List(attributes, values)

  final def list[TA, VA](attributes: VA, values: Value[TA, VA]*)(implicit ev: IsNotAValue[VA]): Value[TA, VA] =
    List(attributes, values: _*)

  final def list(elements: Chunk[RawValue]): RawValue = List.Raw(elements)
  final def list(elements: RawValue*): RawValue       = List.Raw(elements: _*)

  final def listOf[TA](elementType: UType, elements: Value[TA, UType]*): Value[TA, UType] =
    List(listType(elementType), elements: _*)

  final def listOf(elements: RawValue*)(elementType: UType): TypedValue =
    List(listType(elementType), elements.map(e => (e :> elementType)): _*)

  final def literal[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] = Literal(attributes, literal)
  final def literal[A](literal: Lit[A]): RawValue                               = Literal.Raw(literal)
  final def literal(value: String): TypedValue = {
    val literal = Lit.string(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: Int): TypedValue = {
    val literal = Lit.int(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: Long): TypedValue = {
    val literal = Lit.long(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: Float): TypedValue = {
    val literal = Lit.float(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: Double): TypedValue = {
    val literal = Lit.double(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: Boolean): TypedValue = {
    val literal = Lit.boolean(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: scala.BigDecimal): TypedValue = {
    val literal = Lit.decimal(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: java.math.BigDecimal): TypedValue = {
    val literal = Lit.decimal(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: java.math.BigInteger): TypedValue = {
    val literal = Lit.wholeNumber(value)
    Literal(literal.inferredType, literal)
  }

  final def literal(value: scala.BigInt): TypedValue = {
    val literal = Lit.wholeNumber(value)
    Literal(literal.inferredType, literal)
  }

  final def literalTyped[A](literal: Lit[A]): TypedValue = Literal.Typed(literal)

  final def long[A](attributes: A, value: Long): Value[Nothing, A] = Literal(attributes, Lit.long(value))
  final def long(value: Long): RawValue                            = Literal.Raw(Lit.long(value))

  final def patternMatch[TA, VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: Chunk[(Pattern[VA], Value[TA, VA])]
  ): Value[TA, VA] =
    PatternMatch(attributes, branchOutOn, cases)

  final def patternMatch[TA, VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: (Pattern[VA], Value[TA, VA])*
  ): Value[TA, VA] =
    PatternMatch(attributes, branchOutOn, cases: _*)

  final def patternMatch(branchOutOn: RawValue, cases: Chunk[(UPattern, RawValue)]): RawValue =
    PatternMatch.Raw(branchOutOn, cases)

  final def patternMatch(branchOutOn: RawValue, cases: (UPattern, RawValue)*): RawValue =
    PatternMatch.Raw(branchOutOn, cases: _*)

  final def record[TA, VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]): Value[TA, VA] =
    Record(attributes, fields)

  final def record[TA, VA](attributes: VA, fields: Map[Name, Value[TA, VA]]): Value[TA, VA] =
    Record.fromMap(attributes, fields)

  final def record[TA, VA](attributes: VA, fields: (String, Value[TA, VA])*)(implicit
      ev: IsNotAValue[VA]
  ): Value[TA, VA] = Record(attributes, fields: _*)

  final def record(fields: Chunk[(Name, RawValue)]): RawValue = Record.Raw(fields)
  final def record(fields: (String, RawValue)*): RawValue     = Record.Raw(fields: _*)
  final def record(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): RawValue =
    Record.Raw(firstField +: Chunk.fromIterable(otherFields))

  final def reference[A](attributes: A, name: String): Value[Nothing, A] = Reference(attributes, name)
  final def reference[A](attributes: A, name: FQName): Value[Nothing, A] = Reference(attributes, name)
  final def reference(name: String, tpe: UType): TypedValue              = Reference.Typed(tpe, name)
  final def reference(name: FQName, tpe: UType): TypedValue              = Reference.Typed(tpe, name)
  final def reference[A](attributes: A, packageName: String, moduleName: String, localName: String): Value[Nothing, A] =
    Reference(attributes, packageName, moduleName, localName)
  final def reference(name: String): RawValue = Reference.Raw(name)
  final def reference(name: FQName): RawValue = Reference.Raw(name)
  final def reference(packageName: String, moduleName: String, localName: String): RawValue =
    Reference.Raw(packageName, moduleName, localName)

  final def string[VA](attributes: VA, value: String): Value[Nothing, VA] = Literal(attributes, Lit.string(value))
  final def string(value: String): RawValue                               = Literal.Raw(Lit.string(value))

  final def tuple[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] = Tuple(attributes, elements)
  final def tuple[TA, VA](attributes: VA, first: Value[TA, VA], second: Value[TA, VA], otherElements: Value[TA, VA]*)(
      implicit ev: IsNotAValue[VA]
  ): Value[TA, VA] = Tuple(attributes, first +: second +: Chunk.fromIterable(otherElements))

  final def tuple(elements: RawValue*): RawValue       = Tuple.Raw(elements: _*)
  final def tuple(elements: Chunk[RawValue]): RawValue = Tuple.Raw(elements)
  final def tuple(element: (RawValue, UType), elements: (RawValue, UType)*): TypedValue =
    Tuple.Typed(Chunk.fromIterable((element +: elements).map { case (v, t) => v :> t }))

  final val unit: RawValue                            = Unit.Raw()
  final def unit[A](attributes: A): Value[Nothing, A] = Unit(attributes)

  final def update[TA, VA](
      attributes: VA,
      valueToUpdate: Value[TA, VA],
      fields: Chunk[(Name, Value[TA, VA])]
  ): Value[TA, VA] =
    UpdateRecord(attributes, valueToUpdate, fields)

  final def update[TA, VA](
      attributes: VA,
      valueToUpdate: Value[TA, VA],
      fields: (String, Value[TA, VA])*
  ): Value[TA, VA] =
    UpdateRecord(attributes, valueToUpdate, fields: _*)

  final def update(valueToUpdate: RawValue, fields: Chunk[(Name, RawValue)]): RawValue =
    UpdateRecord.Raw(valueToUpdate, fields)

  final def update(valueToUpdate: RawValue, fields: (String, RawValue)*): RawValue =
    UpdateRecord.Raw(valueToUpdate, fields: _*)

  final def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  final def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  final def variable(name: Name): RawValue                              = Variable.Raw(name)
  final def variable(name: String): RawValue                            = Variable.Raw(name)
  final def variable(name: String, tpe: UType): TypedValue              = Variable.Typed(tpe, name)
  final def variable(name: Name, tpe: UType): TypedValue                = Variable.Typed(tpe, name)

  final def wholeNumber(value: java.math.BigInteger): RawValue =
    literal(Lit.wholeNumber(value))

  final def wholeNumber(value: BigInt): RawValue =
    literal(Lit.wholeNumber(value))
}
