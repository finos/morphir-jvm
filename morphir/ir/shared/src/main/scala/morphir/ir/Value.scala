package morphir.ir

sealed abstract class Value extends Product with Serializable

object Value {

  final case class Specification[+A]()
  final case class Definition[+A]()
}
