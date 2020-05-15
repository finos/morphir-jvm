package morphir.ir.codec.`type`

//import cats.syntax.functor._
import io.circe.{ Decoder, Encoder }
//import io.circe.syntax._
import morphir.ir.Type

trait TypeCoproductCodec {
  implicit def encodeTypeCoproduct[A: Encoder]: Encoder[Type[A]] = ???
//
//  implicit def encodeTypeCoproduct[A: Encoder]: Encoder[Type[A]] = Encoder.instance {
//    case variable @ Type.Variable(_, _)                    => variable.asJson
//    case reference @ Type.Reference(_, _, _)               => reference.asJson
//    case tuple @ Type.Tuple(_, _)                          => tuple.asJson
//    case record @ Type.Record(_, _)                        => record.asJson
//    case extensibleRecord @ Type.ExtensibleRecord(_, _, _) => extensibleRecord.asJson
//    case function @ Type.Function(_, _, _)                 => function.asJson
//    case unit @ Type.Unit(_)                               => unit.asJson
//  }

  implicit def decodeTypeCoproduct[A: Decoder]: Decoder[Type[A]] = ???

//  implicit def decodeTypeCoproduct[A: Decoder]: Decoder[Type[A]] =
//    List[Decoder[Type[A]]](
//      Decoder[Type.Variable[A]].widen,
//      Decoder[Type.Reference[A]].widen,
//      Decoder[Type.Tuple[A]].widen,
//      Decoder[Type.Record[A]].widen,
//      Decoder[Type.ExtensibleRecord[A]].widen,
//      Decoder[Type.Function[A]].widen,
//      Decoder[Type.Unit[A]].widen
//    ).reduceLeft(_ or _)
}
