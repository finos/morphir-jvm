package morphir.ir

//import io.circe.{Encoder, Decoder}

final case class TypeParameterList[+A](toList: List[Type[A]]) extends AnyVal
object TypeParameterList {}
