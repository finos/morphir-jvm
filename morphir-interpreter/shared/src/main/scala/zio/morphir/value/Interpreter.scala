package zio.morphir.value

import zio.morphir.ir.Name
import zio.morphir.ir.ValueModule.RawValue
import zio.morphir.ir.TypeModule
import zio.morphir.IRModule.IR
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.ValueModule.ValueCase.*
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.FQName
import zio.morphir.ir.Pattern
import zio.morphir.ir.NativeFunction.*

import zio.Chunk

import scala.collection.immutable.ListMap
import zio.morphir.ir.ValueModule.Value
import zio.morphir.ir.TypeModule.Specification.TypeAliasSpecification
import zio.morphir.IRModule
object Interpreter {

  final case class Variables(map: Map[Name, Result])

  sealed trait Result

  object Result {
    final case class Strict(value: Any) extends Result

    final case class Lazy(
        value: RawValue,
        variables: Map[Name, Result],
        references: Map[FQName, Any],
        definitions: Map[Name, Value[Any]]
    ) extends Result
  }

  type ??? = Nothing

  def evaluate(value: RawValue): Any = evaluate(value, IR.empty, Map.empty)

  def evaluate(value: RawValue, ir: IR, nativeFunctions: Map[FQName, NativeFunction]): Any = {

    // HACK: Just quieting some warnings
    val _ = (ir, nativeFunctions)

    def loop(
        value: RawValue,
        variables: Map[Name, Result],
        references: Map[FQName, Any]
    ): Any = {
      value.caseValue match {

        case NativeApplyCase(function, args) =>
          evalNativeFunction(function, args.map(loop(_, variables, references)))

        case ApplyCase(function, arguments) =>
          val scalaFunction      = loop(function, variables, references)
          val evaluatedArguments = arguments.map(loop(_, variables, references))
          applyFunction(scalaFunction, evaluatedArguments)

        // ConstructorCase("Person")

        // function("Adam", 42)

        /**
         * type Employee
         * = Worker { name: String tenure: Int }
         * | Manager { name: String tenure: Int bonus: Int }
         * | Worker2 String Int ConstructorCase("Worker") ConstructorCase("Manager") ConstructorCase("Worker2")
         */

        case ConstructorCase(fqName) =>
          println(s"evaluating: ConstructorCase($fqName)")
          val dealiased = ir.resolveAliases(fqName)
          def getRecordConstructor(name: FQName): Option[Any] =
            ir.typeSpecifications.get(name).collect {
              case TypeAliasSpecification(_, TypeModule.Type(TypeModule.TypeCase.RecordCase(fields), _), _) =>
                constructFunction(fqName, fields)
            }

          def getTypeConstructor(name: FQName): Option[Any] =
            ir.typeConstructors.get(name) match {
              case Some(info) => Some(constructConstructor(name, info))
              case None =>
                println(s"Failed to find type constructor for $name")
                None
            }

          getRecordConstructor(dealiased) orElse getTypeConstructor(dealiased) match {
            case Some(fn) => fn
            case None =>
              throw new InterpretationError.TypeNotFound(dealiased.toString)
          }

        // case class Constructor(name FQName)
        // case class ConstructorApply(name : FQName, args : Chunk[Any])

        // final case class ExtensibleRecordCase[+Self](name: Name, fields: Chunk[Field[Self]]) extends TypeCase[Self]
        // final case class FunctionCase[+Self](paramTypes: Chunk[Self], returnType: Self)      extends TypeCase[Self]
        // final case class RecordCase[+Self](fields: Chunk[Field[Self]])                       extends TypeCase[Self]
        // final case class ReferenceCase[+Self](typeName: FQName, typeParams: Chunk[Self])     extends TypeCase[Self]
        // final case class TupleCase[+Self](elementTypes: Chunk[Self])                         extends TypeCase[Self]
        // case object UnitCase                                                                 extends TypeCase[Nothing]
        // final case class VariableCase(name: Name)                                            extends TypeCase[Nothing]

        // TupleCase === Scala tuple (ordered values but no names)
        // RecordCase === Python-style named tuple (ordered values and names)

        // FieldCase(Apply(Constructor(Person), "Adam", 42), "age")

        // Constructor = Chunk[Any] => Generic

        case FieldCase(target, name) =>
          val record = loop(target, variables, references).asInstanceOf[ListMap[Name, Any]]
          record.get(name) match {
            case Some(value) => value
            case None =>
              throw new InterpretationError.FieldNotFound(name, s"Field $name not found in $record")
          }

        case FieldFunctionCase(name) =>
          (input: Any) =>
            input match {
              case record: ListMap[_, _] =>
                record.asInstanceOf[ListMap[Name, Any]].get(name) match {
                  case Some(fieldValue) => fieldValue
                  case None             => InterpretationError.FieldNotFound(name, s"Field $name not found in $input")
                }
              case _ => throw new InterpretationError.RecordExpected(s"Record expected but got $input")
            }

        case IfThenElseCase(condition, thenBranch, elseBranch) =>
          if (loop(condition, variables, references).asInstanceOf[Boolean]) {
            loop(thenBranch, variables, references)
          } else {
            loop(elseBranch, variables, references)
          }

        case ListCase(elements) =>
          elements.map(loop(_, variables, references)).toList

        case LiteralCase(literal) =>
          evalLiteralValue(literal)

        case PatternMatchCase(branchOutOn, cases) =>
          val evaluatedBody                   = loop(branchOutOn, variables, references)
          val casesChunk                      = cases
          var i                               = 0
          val length                          = casesChunk.length
          var rightHandSide: RawValue         = null
          var newVariables: Map[Name, Result] = Map.empty
          while (i < length) {
            matchPattern(evaluatedBody, casesChunk(i)._1) match {
              case MatchResult.Success(variables) =>
                rightHandSide = casesChunk(i)._2
                newVariables = variables.map { case (key, value) => key -> Result.Strict(value) }
                i = length
              case MatchResult.Failure(_, _) =>
                i += 1
            }
          }

          if (rightHandSide eq null) throw new InterpretationError.MatchError("didn't match")
          else loop(rightHandSide, variables ++ newVariables, references)

        case RecordCase(fields) =>
          val values = fields.map { case (name, value) =>
            name -> loop(value, variables, references)
          }
          ListMap(values: _*)

        case ReferenceCase(name) =>
          references.get(name) match {
            case Some(value) => value

            case None => throw new InterpretationError.ReferenceNotFound(name, s"Reference $name not found")
          }

        case TupleCase(elements) =>
          evalTuple(elements.map(loop(_, variables, references)))

        case UnitCase =>
          ()

        case VariableCase(name) =>
          variables.get(name) match {
            case Some(Result.Strict(value)) => value
            case Some(Result.Lazy(value, variables, references, definitions)) => {
              def shallow = definitions.map { case (key, value) =>
                key -> Result.Lazy(value, variables, references, definitions)
              }
              loop(value, variables ++ shallow, references)
            }
            case None => throw new InterpretationError.VariableNotFound(name, s"Variable $name not found")
          }

        case LetDefinitionCase(name, value, body) =>
          loop(
            body,
            variables + (name -> Result.Strict(loop(value.toValue, variables, references))),
            references
          )

        case LetRecursionCase(valueDefinitions, inValue) =>
          def shallow = valueDefinitions.map { case (key, value) =>
            key -> Result.Lazy(
              value.toValue,
              variables,
              references,
              valueDefinitions.map { case (k, v) => k -> v.toValue }
            )
          }

          loop(
            inValue,
            variables ++ shallow,
            references
          )

        case UpdateRecordCase(valueToUpdate, fieldsToUpdate) =>
          val evaluatedValueToUpdate = loop(valueToUpdate, variables, references)
          val evaluatedFieldsToUpdate = fieldsToUpdate.map { case (name, value) =>
            name -> loop(value, variables, references)
          }
          evaluatedValueToUpdate match {
            case record: ListMap[_, _] =>
              val newRecord = record.asInstanceOf[ListMap[Name, Any]] ++ evaluatedFieldsToUpdate.toMap
              newRecord
            case _ =>
              throw new InterpretationError.RecordExpected(
                s"Record expected but got $evaluatedValueToUpdate"
              )
          }

        case LambdaCase(argumentPattern, body) =>
          (input: Any) =>
            matchPattern(input, argumentPattern) match {
              case MatchResult.Success(newVariables) =>
                loop(
                  body,
                  variables ++ newVariables.map { case (key, value) => key -> Result.Strict(value) },
                  references
                )
              case MatchResult.Failure(pattern, input) =>
                throw new InterpretationError.MatchError(
                  s"Pattern $pattern didn't match input $input"
                )
            }

        case DestructureCase(pattern, valueToDestruct, inValue) =>
          val evaluatedValueToDestruct = loop(valueToDestruct, variables, references)
          matchPattern(evaluatedValueToDestruct, pattern) match {
            case MatchResult.Success(newVariables) =>
              loop(
                inValue,
                variables ++ newVariables.map { case (key, value) => key -> Result.Strict(value) },
                references
              )
            case MatchResult.Failure(pattern, input) =>
              throw new InterpretationError.MatchError(
                s"Pattern $pattern didn't match input $input"
              )
          }
      }
    }

    try {
      Right(loop(value, Map.empty, Map.empty))
    } catch {
      case interpretationError: InterpretationError => Left(interpretationError)
    }
  }

  sealed trait MatchResult
  object MatchResult {
    final case class Failure(body: Any, caseStatement: Pattern[Any]) extends MatchResult
    final case class Success(variables: Map[Name, Any])              extends MatchResult
  }

  def matchPattern(body: Any, caseStatement: Pattern[Any]): MatchResult = {
    val unitValue: Unit = ()
    val err             = MatchResult.Failure(body, caseStatement)
    caseStatement match {
      case Pattern.AsPattern(pattern, name, _) =>
        matchPattern(body, pattern) match {
          case MatchResult.Success(_) =>
            MatchResult.Success(Map.empty + (name -> body))
          case x: MatchResult.Failure => x
        }
      case Pattern.ConstructorPattern(_, _, _) =>
        ???
      case Pattern.EmptyListPattern(_) =>
        if (body == Nil) MatchResult.Success(Map.empty) else err
      case Pattern.HeadTailPattern(headPattern, tailPattern, _) =>
        body match {
          case head :: tail =>
            val headMatchResult = matchPattern(head, headPattern)
            val tailMatchResult = matchPattern(tail, tailPattern)
            (headMatchResult, tailMatchResult) match {
              case (MatchResult.Success(headVariables), MatchResult.Success(tailVariables)) =>
                MatchResult.Success(headVariables ++ tailVariables)
              case (failure: MatchResult.Failure, _) => failure
              case (_, failure: MatchResult.Failure) => failure
            }
          case _ => err
        }
      case Pattern.LiteralPattern(literal, _) =>
        if (body == literal.value) MatchResult.Success(Map.empty) else err
      case Pattern.UnitPattern(_) =>
        if (body == unitValue) MatchResult.Success(Map.empty) else err
      case Pattern.TuplePattern(patterns, _) =>
        def helper(remainingBody: List[Any], remainingPattern: List[Pattern[_]]): MatchResult =
          (remainingBody, remainingPattern) match {
            case (Nil, Nil) => MatchResult.Success(Map.empty)
            case (_, Nil)   => err
            case (Nil, _)   => err
            case (b :: bs, t :: ts) =>
              (helper(bs, ts), matchPattern(b, t)) match {
                case (MatchResult.Success(m1), MatchResult.Success(m2)) => MatchResult.Success(m1 ++ m2)
                case _                                                  => err
              }
          }

        try {
          helper(tupleToChunk(body).toList, patterns.toList)
        } catch {
          case _: InterpretationError.MatchError => err
        }
      case Pattern.WildcardPattern(_) =>
        MatchResult.Success(Map.empty)
    }
  }

  private def evalLiteralValue(literalValue: LiteralValue): Any =
    literalValue match {
      case LiteralValue.Bool(value)        => value
      case LiteralValue.Char(value)        => value
      case LiteralValue.String(value)      => value
      case LiteralValue.WholeNumber(value) => value
      case LiteralValue.Float(value)       => value
    }

  private def evalNativeFunction(function: NativeFunction, args: Chunk[Any]): Any =
    function match {
      case Addition    => evalAddition(args)
      case Subtraction => evalSubtraction(args)
    }

  private def evalAddition(args: Chunk[Any]): Any =
    if (args.length == 0)
      throw new InterpretationError.InvalidArguments(args, s"Addition expected at least two argument but got none.")
    else if (args(0).isInstanceOf[java.math.BigInteger])
      args.asInstanceOf[Chunk[java.math.BigInteger]].reduce(_ add _)
    else
      args.asInstanceOf[Chunk[java.math.BigDecimal]].reduce(_ add _)

  private def evalSubtraction(args: Chunk[Any]): Any =
    if (args.length != 2)
      throw new InterpretationError.InvalidArguments(args, s"Subtraction expected exactly two arguments.")
    else if (args(0).isInstanceOf[java.math.BigInteger])
      args(0).asInstanceOf[java.math.BigInteger] subtract args(1).asInstanceOf[java.math.BigInteger]
    else
      args(0).asInstanceOf[java.math.BigDecimal] subtract args(1).asInstanceOf[java.math.BigDecimal]

  // format: off
  private def evalTuple(value: Chunk[Any]): Any =
    value.toList match {
      case a :: Nil => Tuple1(a)
      case a :: b :: Nil => (a, b)
      case a :: b :: c :: Nil => (a, b, c)
      case a :: b :: c :: d :: Nil => (a, b, c, d)
      case a :: b :: c :: d :: e :: Nil => (a, b, c, d, e)
      case a :: b :: c :: d :: e :: f :: Nil => (a, b, c, d, e, f)
      case a :: b :: c :: d :: e :: f :: g :: Nil => (a, b, c, d, e, f, g)
      case a :: b :: c :: d :: e :: f :: g :: h :: Nil => (a, b, c, d, e, f, g, h)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: Nil => (a, b, c, d, e, f, g, h, i)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: Nil => (a, b, c, d, e, f, g, h, i, j)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: Nil => (a, b, c, d, e, f, g, h, i, j, k)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: q :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: q :: r :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: q :: r :: s :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: q :: r :: s :: t :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: q :: r :: s :: t :: u :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
      case a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: p :: q :: r :: s :: t :: u :: v :: Nil => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
      case _ => throw new InterpretationError.TupleTooLong(value.length)
    }
    // format: on

  private def tupleToChunk(tuple: Any): Chunk[Any] =
    tuple match {
      case Tuple1(a)                                        => Chunk(a)
      case (a, b)                                           => Chunk(a, b)
      case (a, b, c)                                        => Chunk(a, b, c)
      case (a, b, c, d)                                     => Chunk(a, b, c, d)
      case (a, b, c, d, e)                                  => Chunk(a, b, c, d, e)
      case (a, b, c, d, e, f)                               => Chunk(a, b, c, d, e, f)
      case (a, b, c, d, e, f, g)                            => Chunk(a, b, c, d, e, f, g)
      case (a, b, c, d, e, f, g, h)                         => Chunk(a, b, c, d, e, f, g, h)
      case (a, b, c, d, e, f, g, h, i)                      => Chunk(a, b, c, d, e, f, g, h, i)
      case (a, b, c, d, e, f, g, h, i, j)                   => Chunk(a, b, c, d, e, f, g, h, i, j)
      case (a, b, c, d, e, f, g, h, i, j, k)                => Chunk(a, b, c, d, e, f, g, h, i, j, k)
      case (a, b, c, d, e, f, g, h, i, j, k, l)             => Chunk(a, b, c, d, e, f, g, h, i, j, k, l)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m)          => Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n)       => Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)    => Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
        Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
        Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
        Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
        Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
        Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
        Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
      case _ => throw new InterpretationError.MatchError("value was not a tuple")
    }

  def applyFunction(function: Any, arguments: Chunk[Any]): Any =
    function match {
      case f: Function1[_, _]    => f.asInstanceOf[Function1[Any, Any]](arguments(0))
      case f: Function2[_, _, _] => f.asInstanceOf[Function2[Any, Any, Any]](arguments(0), arguments(1))
      case _                     => throw new Exception("more than two arguments not currently supported")
    }

  private def fqNameToGenericCaseClassName(fqName: FQName): String =
    GenericCaseClass.fqNameToGenericCaseClassName(fqName)

  private def nameToFieldName(name: Name): String =
    GenericCaseClass.nameToFieldName(name)

  private def constructConstructor(name: FQName, info: IRModule.TypeConstructorInfo): Any = {
    println(s"Creating constructor for $name: with info $info")
    info.typeArgs.length match {
      case 1 =>
        new Function[Any, Any] {
          override def apply(v1: Any): Any =
            GenericCaseClass(fqNameToGenericCaseClassName(name), ListMap(nameToFieldName(info.typeArgs(0)._1) -> v1))
        }
      case 2 =>
        new Function2[Any, Any, Any] {
          override def apply(v1: Any, v2: Any): Any = {
            GenericCaseClass(
              fqNameToGenericCaseClassName(name),
              ListMap(nameToFieldName(info.typeArgs(0)._1) -> v1, nameToFieldName(info.typeArgs(1)._1) -> v2)
            )
          }
        }
      case 3 =>
        new Function3[Any, Any, Any, Any] {
          override def apply(v1: Any, v2: Any, v3: Any): Any = {
            GenericCaseClass(
              fqNameToGenericCaseClassName(name),
              ListMap(
                nameToFieldName(info.typeArgs(0)._1) -> v1,
                nameToFieldName(info.typeArgs(1)._1) -> v2,
                nameToFieldName(info.typeArgs(2)._1) -> v3
              )
            )
          }
        }
      case _ => throw InterpretationError.ConstructorError(name, info.typeArgs)
    }
  }

  private def constructFunction(name: FQName, fields: Chunk[TypeModule.Field[TypeModule.Type[Any]]]): Any =
    fields.length match {
      case 1 =>
        new Function1[Any, Any] {
          override def apply(v1: Any): Any =
            GenericCaseClass(fqNameToGenericCaseClassName(name), ListMap(nameToFieldName(fields(0).name) -> v1))
        }
      case 2 =>
        new Function2[Any, Any, Any] {
          override def apply(v1: Any, v2: Any): Any = {
            println(s"v1 - $v1")
            println(s"v2 - $v2")
            GenericCaseClass(
              fqNameToGenericCaseClassName(name),
              ListMap(nameToFieldName(fields(0).name) -> v1, nameToFieldName(fields(1).name) -> v2)
            )
          }
        }
      case 3 =>
        new Function3[Any, Any, Any, Any] {
          override def apply(v1: Any, v2: Any, v3: Any): Any = {
            GenericCaseClass(
              fqNameToGenericCaseClassName(name),
              ListMap(
                nameToFieldName(fields(0).name) -> v1,
                nameToFieldName(fields(1).name) -> v2,
                nameToFieldName(fields(2).name) -> v3
              )
            )
          }
        }
      case _ => throw new Exception("more than two arguments not currently supported")
    }
}

sealed trait InterpretationError extends Throwable
object InterpretationError {
  final case class Message(message: String)                            extends InterpretationError
  final case class VariableNotFound(name: Name, message: String)       extends InterpretationError
  final case class ReferenceNotFound(name: FQName, message: String)    extends InterpretationError
  final case class RecordExpected(message: String)                     extends InterpretationError
  final case class InvalidArguments(args: Chunk[Any], message: String) extends InterpretationError
  final case class TupleTooLong(length: Int)                           extends InterpretationError
  final case class FieldNotFound(name: Name, message: String)          extends InterpretationError
  final case class MatchError(mesage: String)                          extends InterpretationError
  final case class TypeNotFound(message: String)                       extends InterpretationError
  final case class ConstructorError(name: FQName, args: Chunk[Any])    extends InterpretationError
}

case class GenericCaseClass(name: String, fields: ListMap[String, Any])
object GenericCaseClass {
  def fromFields(fqName: FQName, fields: (Name, Any)*): GenericCaseClass =
    GenericCaseClass(
      fqNameToGenericCaseClassName(fqName),
      ListMap(fields.map { case (name, value) => nameToFieldName(name) -> value }: _*)
    )

  def fqNameToGenericCaseClassName(fqName: FQName): String =
    fqName.toString

  def nameToFieldName(name: Name): String =
    name.toString

  def named(name: FQName) = ???
}

// To Do List:
// // Tests:
// EmptyList
// Head, Tail (when done) CHECK
// Unit
// Tuple1 (and ensure does not match everything?)
// // Ergonomics: CHECK
// Head tail CHECK
// Constructor/Placeholder ??
// // Lower level
// Lambda case CHECK
// Update record case CHECK
// Destructure case CHECK
// // Higher level?
// Custom types/Actual constructor
// Actual recursive definitions CHECK

// fib i = (if i == 0 || i == 1) 1 else fib (i-1) + fib(i-2)

// nonrec_fib f i = (if i == 0 || i == 1) 1 else f f (i-1) + f f (i - 2)
// nonrec_fib nonrec_fib i
