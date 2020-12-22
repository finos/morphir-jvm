package morphir.flowz
import zio.prelude._
package object eventing {

  object AggregateId extends Newtype[String]
  type AggregateId = AggregateId.Type
}
