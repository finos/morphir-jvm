package morphir.dependency.dag

/** Generated based on Dependency.DAG
*/
object Codec{

  implicit def encodeCycleDetected[ComparableNode](
    encodeComparableNode: io.circe.Encoder[ComparableNode]
  ): io.circe.Encoder[morphir.dependency.DAG.CycleDetected[ComparableNode]] =
    ((cycleDetected: morphir.dependency.DAG.CycleDetected[ComparableNode]) =>
      cycleDetected match {
        case morphir.dependency.DAG.CycleDetected(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("CycleDetected"),
            encodeComparableNode(arg1),
            encodeComparableNode(arg2)
          )
      })
  
  implicit def encodeDAG[ComparableNode](
    encodeComparableNode: io.circe.Encoder[ComparableNode]
  ): io.circe.Encoder[morphir.dependency.DAG.DAG[ComparableNode]] =
    ((dAG: morphir.dependency.DAG.DAG[ComparableNode]) =>
      dAG match {
        case morphir.dependency.DAG.DAG(arg1) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("DAG"),
            morphir.sdk.dict.Codec.encodeDict(
              encodeComparableNode,
              morphir.sdk.set.Codec.encodeSet(encodeComparableNode)
            )(arg1)
          )
      })
  
  implicit def decodeCycleDetected[ComparableNode](
    decodeComparableNode: io.circe.Decoder[ComparableNode]
  ): io.circe.Decoder[morphir.dependency.DAG.CycleDetected[ComparableNode]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case "CycleDetected" => 
            for {
              arg1 <- c.downN(1).as(decodeComparableNode)
              arg2 <- c.downN(2).as(decodeComparableNode)
            }  yield morphir.dependency.DAG.CycleDetected(
              arg1,
              arg2
            )
        })))
  
  implicit def decodeDAG[ComparableNode](
    decodeComparableNode: io.circe.Decoder[ComparableNode]
  ): io.circe.Decoder[morphir.dependency.DAG.DAG[ComparableNode]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case "DAG" => 
            for {
              arg1 <- c.downN(1).as(morphir.sdk.dict.Codec.decodeDict(
                decodeComparableNode,
                morphir.sdk.set.Codec.decodeSet(decodeComparableNode)
              ))
            }  yield morphir.dependency.DAG.DAG(arg1)
        })))

}