package morphir.ir

sealed abstract class Literal[+A] extends Product with Serializable
object Literal {
  final case class BoolLiteral(value: Boolean)  extends Literal[Boolean]
  final case class CharLiteral(value: Char)     extends Literal[Char]
  final case class StringLiteral(value: String) extends Literal[String]
  final case class IntLiteral(value: Int)       extends Literal[Int]
  final case class FloatLiteral(value: Float)   extends Literal[Float]
}
