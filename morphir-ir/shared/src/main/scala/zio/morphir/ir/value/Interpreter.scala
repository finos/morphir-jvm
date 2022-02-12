package zio.morphir.ir.value
import zio.morphir.ir.Name
import zio.morphir.ir.ValueModule.RawValue
import zio.morphir.IRModule.IR
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.ValueModule.ValueCase.*
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.FQName
import zio.morphir.ir.NativeFunction.*

import zio.Chunk

import scala.collection.immutable.ListMap
object Interpreter {

  final case class Variables(toMap: Map[Name, RawValue])

  type ??? = Nothing

  def evaluate(value: RawValue): Any = evaluate(value, IR.empty, Map.empty)

  def evaluate(value: RawValue, ir: IR, nativeFunctions: Map[FQName, NativeFunction]): Any = {

    // HACK: Just quieting some warnings
    val _ = (ir, nativeFunctions)

    def loop(
        value: RawValue,
        variables: Map[Name, Any],
        references: Map[FQName, Any]
    ): Any = {
      value.caseValue match {

        case NativeApplyCase(function, args) =>
          evalNativeFunction(function, args.map(loop(_, variables, references)))

        case ApplyCase(function, arguments) =>
          val scalaFunction      = loop(function, variables, references)
          val evaluatedArguments = arguments.map(loop(_, variables, references))
          applyFunction(scalaFunction, evaluatedArguments)

        case ConstructorCase(_) =>
          ???

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
              case _ => throw new InterpretationError.RecordExpected(name, s"Record expected but got $input")
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
          sealed trait MatchResult
          object MatchResult {
            case object Failure                                 extends MatchResult
            final case class Success(variables: Map[Name, Any]) extends MatchResult
          }
          // Option[Map[Name, Any]]

          def matches(body: Any, caseStatement: RawValue): MatchResult = {
            val unitValue: Unit = ()
            caseStatement.caseValue match {
              case PatternCase.AsPatternCase(pattern, name) =>
                matches(body, pattern) match {
                  case MatchResult.Success(_) =>
                    MatchResult.Success(Map.empty + (name -> body))
                  case MatchResult.Failure =>
                    MatchResult.Failure
                }
              case PatternCase.EmptyListPatternCase =>
                if (body == Nil) MatchResult.Success(Map.empty) else MatchResult.Failure
              case PatternCase.LiteralPatternCase(literal) =>
                if (body == literal) MatchResult.Success(Map.empty) else MatchResult.Failure
              case PatternCase.UnitPatternCase =>
                if (body == unitValue) MatchResult.Success(Map.empty) else MatchResult.Failure
              // case PatternCase.TuplePatternCase(patterns) =>
              //   def helper(remainingBody, remainingTuple) => (remainingTuple, remainingPattern) match
              //    case (Nil, Nil) =>MatchResult.Success(List.Empty)
              //    case (_, Nil) => MatchResult.Failure
              //    case (Nil, _) => MatchResult.Failure
              //    case (b :: bs, t :: ts) =>(helper(bs, ts), matches(b, t)) match
              //      case (MatchResult.Success(m1), MatchResult.Success(m2)) =>MatchResult.Success(m1 + m2)
              //      case _ => MatchResult.Failure
              //   body match
              //    case Tuple(bodyVals) => helper(bodyVals, patterns)
              //    case _ => MatchResult.Failure
              case PatternCase.WildcardPatternCase =>
                MatchResult.Success(Map.empty)
              case _ =>
                throw new InterpretationError.Message("we don't know how to handle this pattern yet")
            }
          }

          val evaluatedBody                = loop(branchOutOn, variables, references)
          val casesChunk                   = cases
          var i                            = 0
          val length                       = casesChunk.length
          var rightHandSide: RawValue      = null
          var newVariables: Map[Name, Any] = Map.empty
          while (i < length) {
            matches(evaluatedBody, casesChunk(i)._1) match {
              case MatchResult.Success(variables) =>
                rightHandSide = casesChunk(i)._2
                newVariables = variables
                i = length
              case MatchResult.Failure =>
                i += 1
            }
          }

          if (rightHandSide eq null) throw new InterpretationError.MatchError("didn't match")
          else loop(rightHandSide, variables ++ newVariables, references)

        // cases = List((x -> doSomething, y -> doSomethingElse)
        // branchOutOn match {
        // case x => doSomething
        // case y => doSomethingElse
        // }
        // }

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
            case Some(value) => value

            case None => throw new InterpretationError.VariableNotFound(name, s"Variable $name not found")
          }

        case LetDefinitionCase(name, value, body) =>
          loop(body, variables + (name -> loop(value, variables, references)), references)

        case LetRecursionCase(valueDefinitions, inValue) =>
          // TODO: handle case where definitions in valueDefinitions refer to each other
          val newVariables: Map[Name, Any] = valueDefinitions.map { case (name, value) =>
            val evaluatedValue = loop(value, variables, references)
            (name, evaluatedValue)
          }
          loop(inValue, variables ++ newVariables, references)

        case UpdateRecordCase(_, _) =>
          ???

        case LambdaCase(argumentPattern, body) =>
          // (input: Any) => loop(Value(PatternMatchCase(input, Chunk(argumentPattern -> body))), variables, references)
          argumentPattern.caseValue match {
            case PatternCase.WildcardPatternCase =>
              (_: Any) => loop(body, variables, references)
            case _ =>
              ???
          }

        // ZIO.succeed(1).map(_ => 2)

        case DestructureCase(_, _, _)                 => ???
        case PatternCase.AsPatternCase(_, _)          => ???
        case PatternCase.ConstructorPatternCase(_, _) => ???
        case PatternCase.EmptyListPatternCase         => ???
        case PatternCase.HeadTailPatternCase(_, _)    => ???
        case PatternCase.LiteralPatternCase(_)        => ???
        case PatternCase.TuplePatternCase(_)          => ???
        case PatternCase.UnitPatternCase              => ???
        case PatternCase.WildcardPatternCase          => ???
      }
    }

    try {
      Right(loop(value, Map.empty, Map.empty))
    } catch {
      case interpretationError: InterpretationError => Left(interpretationError)
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

  def applyFunction(function: Any, arguments: Chunk[Any]): Any =
    function match {
      case f: Function1[_, _]    => f.asInstanceOf[Function1[Any, Any]](arguments(0))
      case f: Function2[_, _, _] => f.asInstanceOf[Function2[Any, Any, Any]](arguments(0), arguments(1))
      case _                     => throw new Exception("more than two arguments not currently supported")
    }
}

sealed trait InterpretationError extends Throwable
object InterpretationError {
  final case class Message(message: String)                            extends InterpretationError
  final case class VariableNotFound(name: Name, message: String)       extends InterpretationError
  final case class ReferenceNotFound(name: FQName, message: String)    extends InterpretationError
  final case class RecordExpected(name: Name, message: String)         extends InterpretationError
  final case class InvalidArguments(args: Chunk[Any], message: String) extends InterpretationError
  final case class TupleTooLong(length: Int)                           extends InterpretationError
  final case class FieldNotFound(name: Name, message: String)          extends InterpretationError
  final case class MatchError(mesage: String)                          extends InterpretationError
}
