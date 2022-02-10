package zio.morphir.ir.value

import scala.collection.immutable.ListMap
import zio.morphir.ir.MorphirIR
import zio.morphir.ir.recursive.PatternCase
import zio.morphir.ir.recursive.ValueCase
import zio.morphir.ir.Literal
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.Name
import zio.Chunk
import zio.morphir.ir.FQName
import zio.morphir.ir.MorphirIR.Value
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.NativeFunction.*

object Interpreter {
  def eval[Annotations](ir: MorphirIR[Annotations]): Either[InterpretationError, Any] = {

    def loop(
        ir: MorphirIR[Annotations],
        variables: Map[Name, Any],
        references: Map[FQName, Value[Annotations]]
    ): Any = {
      ir.caseValue match {

        case ValueCase.ApplyCase(_, _) =>
          ???

        case ValueCase.FieldCase(target, name) =>
          val record = loop(target, variables, references).asInstanceOf[ListMap[Name, Any]]
          record.get(name) match {
            case Some(value) => value
            case None =>
              throw new InterpretationError.FieldNotFound(name, s"Field $name not found in $record")
          }

        case ValueCase.IfThenElseCase(condition, thenCase, elseCase) =>
          if (loop(condition, variables, references).asInstanceOf[Boolean]) {
            loop(thenCase, variables, references)
          } else {
            loop(elseCase, variables, references)
          }

        case ValueCase.LambdaCase(_, _) =>
          ???

        case ValueCase.LetDefinitionCase(name, value, body) =>
          loop(body, variables + (name -> loop(value, variables, references)), references)

        case ValueCase.ListCase(values) =>
          values.map(loop(_, variables, references)).toList

        case ValueCase.LiteralCase(value) =>
          evalLiteralValue(value)

        case ValueCase.NativeApplyCase(function, args) =>
          evalNativeFunction(function, args.map(loop(_, variables, references)))

        // 42 match {
        //   case x => x
        // }

        // PatternMatchChase(42, Map(AsCase("x", WildCaseCase)) -> VariableCase("x"))

        case ValueCase.PatternMatchCase(branchOutOn, cases) =>
          sealed trait MatchResult
          object MatchResult {
            case object Failure                                 extends MatchResult
            final case class Success(variables: Map[Name, Any]) extends MatchResult
          }
          // Option[Map[Name, Any]]

          def matches(body: Any, caseStatement: MorphirIR[Annotations]): MatchResult = {
            val unitValue: Unit = ()
            caseStatement.caseValue match {
              case PatternCase.AsCase(pattern, name) =>
                matches(body, pattern) match {
                  case MatchResult.Success(_) =>
                    MatchResult.Success(Map.empty + (name -> body))
                  case MatchResult.Failure =>
                    MatchResult.Failure
                }
              case PatternCase.EmptyListCase =>
                if (body == Nil) MatchResult.Success(Map.empty) else MatchResult.Failure
              case PatternCase.LiteralCase(literal) =>
                if (body == literal) MatchResult.Success(Map.empty) else MatchResult.Failure
              case PatternCase.UnitCase =>
                if (body == unitValue) MatchResult.Success(Map.empty) else MatchResult.Failure
              case PatternCase.WildcardCase =>
                MatchResult.Success(Map.empty)
              case _ =>
                throw new InterpretationError.Message("we don't know how to handle this pattern yet")
            }
          }

          val evaluatedBody                         = loop(branchOutOn, variables, references)
          val casesChunk                            = cases
          var i                                     = 0
          val length                                = casesChunk.length
          var rightHandSide: MorphirIR[Annotations] = null
          var newVariables: Map[Name, Any]          = Map.empty
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

        case ValueCase.RecordCase(fields) =>
          println(s"Interpreting RecordCase for $fields")
          val values = fields.map { case (name, value) =>
            name -> loop(value, variables, references)
          }
          ListMap(values: _*)

        case ValueCase.ReferenceCase(fqName) =>
          references.get(fqName) match {
            case Some(value) => value
            case None        => throw new InterpretationError.ReferenceNotFound(fqName, s"Reference $fqName not found")
          }

        case ValueCase.TupleCase(value) =>
          evalTuple(value.map(loop(_, variables, references)))

        case ValueCase.UnitCase =>
          ()

        case ValueCase.VariableCase(name) =>
          variables.get(name) match {
            case Some(value) => value

            case None => throw new InterpretationError.VariableNotFound(name, s"Variable $name not found")
          }

        case _ => ???
      }
    }

    try {
      Right(loop(ir, Map.empty, Map.empty))
    } catch {
      case interpretationError: InterpretationError => Left(interpretationError)
    }
  }

  // val x = 1
  // val y = 2
  // x + y

  private def evalLiteralValue(literalValue: LiteralValue): Any =
    literalValue match {
      case Literal.Bool(value)        => value
      case Literal.Char(value)        => value
      case Literal.String(value)      => value
      case Literal.WholeNumber(value) => value
      case Literal.Float(value)       => value
    }

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
}

sealed trait InterpretationError extends Throwable
object InterpretationError {
  final case class Message(message: String)                            extends InterpretationError
  final case class VariableNotFound(name: Name, message: String)       extends InterpretationError
  final case class ReferenceNotFound(name: FQName, message: String)    extends InterpretationError
  final case class InvalidArguments(args: Chunk[Any], message: String) extends InterpretationError
  final case class TupleTooLong(length: Int)                           extends InterpretationError
  final case class FieldNotFound(name: Name, message: String)          extends InterpretationError
  final case class MatchError(mesage: String)                          extends InterpretationError
}
