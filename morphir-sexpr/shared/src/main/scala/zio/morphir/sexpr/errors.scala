package zio.morphir.sexpr

import scala.util.control.NoStackTrace

sealed abstract class SExprError { self =>
  import SExprError.*

  def render: String = self match {
    case Message(text)        => s"($text)"
    case ParseError(text)     => s"($text)"
    case IndexedAccess(index) => s"[$index]"
    case ObjectAccess(field)  => s".$field"
  }
}

object SExprError {
  final case class Message(text: String) extends SExprError

  final case class ParseError(text: String) extends SExprError

  final case class IndexedAccess(index: Int) extends SExprError

  final case class ObjectAccess(field: String) extends SExprError

  def render(trace: List[SExprError]): String = trace.reverse.map(_.render).mkString
}

final case class UnsafeSExpr(trace: List[SExprError])
    extends Exception("If you see this, a developer made a mistake using SExprDecoder")
    with NoStackTrace
