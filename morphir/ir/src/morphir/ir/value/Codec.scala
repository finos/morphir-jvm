package morphir.ir.value

import scala.language.reflectiveCalls

/**
 * Generated based on IR.Value
 */
object Codec {

  implicit def encodeDefinition[Ta, Va](
    encodeTa: io.circe.Encoder[Ta],
    encodeVa: io.circe.Encoder[Va]
  ): io.circe.Encoder[morphir.ir.Value.Definition[Ta, Va]] =
    (
      (definition: {
        def inputTypes: morphir.sdk.List.List[(morphir.ir.Name.Name, Va, morphir.ir.Type.Type[Ta])];
        def outputType: morphir.ir.Type.Type[Ta]; def body: morphir.ir.Value.Value[Ta, Va]
      }) =>
        io.circe.Json.obj(
          (
            "inputTypes",
            morphir.sdk.list.Codec.encodeList(
              (
                (definition: (morphir.ir.Name.Name, Va, morphir.ir.Type.Type[Ta])) =>
                  io.circe.Json.arr(
                    morphir.ir.name.Codec.encodeName(definition._1),
                    encodeVa(definition._2),
                    morphir.ir._type.Codec.encodeType(encodeTa)(definition._3)
                  )
              )
            )(definition.inputTypes)
          ),
          ("outputType", morphir.ir._type.Codec.encodeType(encodeTa)(definition.outputType)),
          (
            "body",
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(definition.body)
          )
        )
    )

  implicit def encodePattern[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Value.Pattern[A]] =
    (
      (pattern: morphir.ir.Value.Pattern[A]) =>
        pattern match {
          case morphir.ir.Value.AsPattern(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("AsPattern"),
              encodeA(arg1),
              morphir.ir.value.Codec.encodePattern(encodeA)(arg2),
              morphir.ir.name.Codec.encodeName(arg3)
            )
          case morphir.ir.Value.ConstructorPattern(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("ConstructorPattern"),
              encodeA(arg1),
              morphir.ir.fqname.Codec.encodeFQName(arg2),
              morphir.sdk.list.Codec.encodeList(morphir.ir.value.Codec.encodePattern(encodeA))(arg3)
            )
          case morphir.ir.Value.EmptyListPattern(arg1) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("EmptyListPattern"),
              encodeA(arg1)
            )
          case morphir.ir.Value.HeadTailPattern(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("HeadTailPattern"),
              encodeA(arg1),
              morphir.ir.value.Codec.encodePattern(encodeA)(arg2),
              morphir.ir.value.Codec.encodePattern(encodeA)(arg3)
            )
          case morphir.ir.Value.LiteralPattern(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("LiteralPattern"),
              encodeA(arg1),
              morphir.ir.literal.Codec.encodeLiteral(arg2)
            )
          case morphir.ir.Value.TuplePattern(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("TuplePattern"),
              encodeA(arg1),
              morphir.sdk.list.Codec.encodeList(morphir.ir.value.Codec.encodePattern(encodeA))(arg2)
            )
          case morphir.ir.Value.UnitPattern(arg1) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("UnitPattern"),
              encodeA(arg1)
            )
          case morphir.ir.Value.WildcardPattern(arg1) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("WildcardPattern"),
              encodeA(arg1)
            )
        }
    )

  implicit val encodeRawValue: io.circe.Encoder[morphir.ir.Value.RawValue] = morphir.ir.value.Codec.encodeValue(
    morphir.sdk.basics.Codec.encodeUnit,
    morphir.sdk.basics.Codec.encodeUnit
  )

  implicit def encodeSpecification[Ta](
    encodeTa: io.circe.Encoder[Ta]
  ): io.circe.Encoder[morphir.ir.Value.Specification[Ta]] =
    (
      (specification: {
        def inputs: morphir.sdk.List.List[(morphir.ir.Name.Name, morphir.ir.Type.Type[Ta])];
        def output: morphir.ir.Type.Type[Ta]
      }) =>
        io.circe.Json.obj(
          (
            "inputs",
            morphir.sdk.list.Codec.encodeList(
              (
                (specification: (morphir.ir.Name.Name, morphir.ir.Type.Type[Ta])) =>
                  io.circe.Json.arr(
                    morphir.ir.name.Codec.encodeName(specification._1),
                    morphir.ir._type.Codec.encodeType(encodeTa)(specification._2)
                  )
              )
            )(specification.inputs)
          ),
          ("output", morphir.ir._type.Codec.encodeType(encodeTa)(specification.output))
        )
    )

  implicit val encodeTypedValue: io.circe.Encoder[morphir.ir.Value.TypedValue] = morphir.ir.value.Codec.encodeValue(
    morphir.sdk.basics.Codec.encodeUnit,
    morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
  )

  implicit def encodeValue[Ta, Va](
    encodeTa: io.circe.Encoder[Ta],
    encodeVa: io.circe.Encoder[Va]
  ): io.circe.Encoder[morphir.ir.Value.Value[Ta, Va]] =
    (
      (value: morphir.ir.Value.Value[Ta, Va]) =>
        value match {
          case morphir.ir.Value.Apply(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Apply"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg2),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg3)
            )
          case morphir.ir.Value.Constructor(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Constructor"),
              encodeVa(arg1),
              morphir.ir.fqname.Codec.encodeFQName(arg2)
            )
          case morphir.ir.Value.Destructure(arg1, arg2, arg3, arg4) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Destructure"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodePattern(encodeVa)(arg2),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg3),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg4)
            )
          case morphir.ir.Value.Field(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Field"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg2),
              morphir.ir.name.Codec.encodeName(arg3)
            )
          case morphir.ir.Value.FieldFunction(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("FieldFunction"),
              encodeVa(arg1),
              morphir.ir.name.Codec.encodeName(arg2)
            )
          case morphir.ir.Value.IfThenElse(arg1, arg2, arg3, arg4) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("IfThenElse"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg2),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg3),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg4)
            )
          case morphir.ir.Value.Lambda(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Lambda"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodePattern(encodeVa)(arg2),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg3)
            )
          case morphir.ir.Value.LetDefinition(arg1, arg2, arg3, arg4) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("LetDefinition"),
              encodeVa(arg1),
              morphir.ir.name.Codec.encodeName(arg2),
              morphir.ir.value.Codec.encodeDefinition(
                encodeTa,
                encodeVa
              )(arg3),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg4)
            )
          case morphir.ir.Value.LetRecursion(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("LetRecursion"),
              encodeVa(arg1),
              morphir.sdk.dict.Codec.encodeDict(
                morphir.ir.name.Codec.encodeName,
                morphir.ir.value.Codec.encodeDefinition(
                  encodeTa,
                  encodeVa
                )
              )(arg2),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg3)
            )
          case morphir.ir.Value.List(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("List"),
              encodeVa(arg1),
              morphir.sdk.list.Codec.encodeList(
                morphir.ir.value.Codec.encodeValue(
                  encodeTa,
                  encodeVa
                )
              )(arg2)
            )
          case morphir.ir.Value.Literal(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Literal"),
              encodeVa(arg1),
              morphir.ir.literal.Codec.encodeLiteral(arg2)
            )
          case morphir.ir.Value.PatternMatch(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("PatternMatch"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg2),
              morphir.sdk.list.Codec.encodeList(
                (
                  (arg3: (morphir.ir.Value.Pattern[Va], morphir.ir.Value.Value[Ta, Va])) =>
                    io.circe.Json.arr(
                      morphir.ir.value.Codec.encodePattern(encodeVa)(arg3._1),
                      morphir.ir.value.Codec.encodeValue(
                        encodeTa,
                        encodeVa
                      )(arg3._2)
                    )
                )
              )(arg3)
            )
          case morphir.ir.Value.Record(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Record"),
              encodeVa(arg1),
              morphir.sdk.dict.Codec.encodeDict(
                morphir.ir.name.Codec.encodeName,
                morphir.ir.value.Codec.encodeValue(
                  encodeTa,
                  encodeVa
                )
              )(arg2)
            )
          case morphir.ir.Value.Reference(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Reference"),
              encodeVa(arg1),
              morphir.ir.fqname.Codec.encodeFQName(arg2)
            )
          case morphir.ir.Value.Tuple(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Tuple"),
              encodeVa(arg1),
              morphir.sdk.list.Codec.encodeList(
                morphir.ir.value.Codec.encodeValue(
                  encodeTa,
                  encodeVa
                )
              )(arg2)
            )
          case morphir.ir.Value.Unit(arg1) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Unit"),
              encodeVa(arg1)
            )
          case morphir.ir.Value.UpdateRecord(arg1, arg2, arg3) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("UpdateRecord"),
              encodeVa(arg1),
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )(arg2),
              morphir.sdk.dict.Codec.encodeDict(
                morphir.ir.name.Codec.encodeName,
                morphir.ir.value.Codec.encodeValue(
                  encodeTa,
                  encodeVa
                )
              )(arg3)
            )
          case morphir.ir.Value.Variable(arg1, arg2) =>
            io.circe.Json.arr(
              io.circe.Json.fromString("Variable"),
              encodeVa(arg1),
              morphir.ir.name.Codec.encodeName(arg2)
            )
        }
    )

  implicit def decodeDefinition[Ta, Va](
    decodeTa: io.circe.Decoder[Ta],
    decodeVa: io.circe.Decoder[Va]
  ): io.circe.Decoder[morphir.ir.Value.Definition[Ta, Va]] =
    (
      (c: io.circe.HCursor) =>
        for {
          inputTypes_ <- c.downField("inputTypes")
                           .as(
                             morphir.sdk.list.Codec.decodeList(
                               (
                                 (c: io.circe.HCursor) =>
                                   for {
                                     arg1 <- c.downN(0).as(morphir.ir.name.Codec.decodeName)
                                     arg2 <- c.downN(1).as(decodeVa)
                                     arg3 <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeTa))
                                   } yield (arg1, arg2, arg3)
                               )
                             )
                           )
          outputType_ <- c.downField("outputType").as(morphir.ir._type.Codec.decodeType(decodeTa))
          body_ <- c.downField("body")
                     .as(
                       morphir.ir.value.Codec.decodeValue(
                         decodeTa,
                         decodeVa
                       )
                     )
        } yield morphir.ir.Value.Definition(
          inputTypes_,
          outputType_,
          body_
        )
    )

  implicit def decodePattern[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Value.Pattern[A]] =
    (
      (c: io.circe.HCursor) =>
        c.withFocus(_.withString(((str) => io.circe.Json.arr(io.circe.Json.fromString(str)))))
          .downN(0)
          .as(morphir.sdk.string.Codec.decodeString)
          .flatMap(
            (
              (tag) =>
                tag match {
                  case "AsPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                      arg2 <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeA))
                      arg3 <- c.downN(3).as(morphir.ir.name.Codec.decodeName)
                    } yield morphir.ir.Value.AsPattern(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "ConstructorPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                      arg2 <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
                      arg3 <-
                        c.downN(3).as(morphir.sdk.list.Codec.decodeList(morphir.ir.value.Codec.decodePattern(decodeA)))
                    } yield morphir.ir.Value.ConstructorPattern(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "EmptyListPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                    } yield morphir.ir.Value.EmptyListPattern(arg1)
                  case "HeadTailPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                      arg2 <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeA))
                      arg3 <- c.downN(3).as(morphir.ir.value.Codec.decodePattern(decodeA))
                    } yield morphir.ir.Value.HeadTailPattern(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "LiteralPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                      arg2 <- c.downN(2).as(morphir.ir.literal.Codec.decodeLiteral)
                    } yield morphir.ir.Value.LiteralPattern(
                      arg1,
                      arg2
                    )
                  case "TuplePattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                      arg2 <-
                        c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir.value.Codec.decodePattern(decodeA)))
                    } yield morphir.ir.Value.TuplePattern(
                      arg1,
                      arg2
                    )
                  case "UnitPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                    } yield morphir.ir.Value.UnitPattern(arg1)
                  case "WildcardPattern" =>
                    for {
                      arg1 <- c.downN(1).as(decodeA)
                    } yield morphir.ir.Value.WildcardPattern(arg1)
                }
            )
          )
    )

  implicit val decodeRawValue: io.circe.Decoder[morphir.ir.Value.RawValue] = morphir.ir.value.Codec.decodeValue(
    morphir.sdk.basics.Codec.decodeUnit,
    morphir.sdk.basics.Codec.decodeUnit
  )

  implicit def decodeSpecification[Ta](
    decodeTa: io.circe.Decoder[Ta]
  ): io.circe.Decoder[morphir.ir.Value.Specification[Ta]] =
    (
      (c: io.circe.HCursor) =>
        for {
          inputs_ <- c.downField("inputs")
                       .as(
                         morphir.sdk.list.Codec.decodeList(
                           (
                             (c: io.circe.HCursor) =>
                               for {
                                 arg1 <- morphir.ir.name.Codec.decodeName(c)
                                 arg2 <- morphir.ir._type.Codec.decodeType(decodeTa)(c)
                               } yield (arg1, arg2)
                           )
                         )
                       )
          output_ <- c.downField("output").as(morphir.ir._type.Codec.decodeType(decodeTa))
        } yield morphir.ir.Value.Specification(
          inputs_,
          output_
        )
    )

  implicit val decodeTypedValue: io.circe.Decoder[morphir.ir.Value.TypedValue] = morphir.ir.value.Codec.decodeValue(
    morphir.sdk.basics.Codec.decodeUnit,
    morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
  )

  implicit def decodeValue[Ta, Va](
    decodeTa: io.circe.Decoder[Ta],
    decodeVa: io.circe.Decoder[Va]
  ): io.circe.Decoder[morphir.ir.Value.Value[Ta, Va]] =
    (
      (c: io.circe.HCursor) =>
        c.withFocus(_.withString(((str) => io.circe.Json.arr(io.circe.Json.fromString(str)))))
          .downN(0)
          .as(morphir.sdk.string.Codec.decodeString)
          .flatMap(
            (
              (tag) =>
                tag match {
                  case "Apply" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                    } yield morphir.ir.Value.Apply(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "Constructor" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
                    } yield morphir.ir.Value.Constructor(
                      arg1,
                      arg2
                    )
                  case "Destructure" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeVa))
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg4 <- c.downN(4)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                    } yield morphir.ir.Value.Destructure(
                      arg1,
                      arg2,
                      arg3,
                      arg4
                    )
                  case "Field" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg3 <- c.downN(3).as(morphir.ir.name.Codec.decodeName)
                    } yield morphir.ir.Value.Field(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "FieldFunction" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
                    } yield morphir.ir.Value.FieldFunction(
                      arg1,
                      arg2
                    )
                  case "IfThenElse" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg4 <- c.downN(4)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                    } yield morphir.ir.Value.IfThenElse(
                      arg1,
                      arg2,
                      arg3,
                      arg4
                    )
                  case "Lambda" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeVa))
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                    } yield morphir.ir.Value.Lambda(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "LetDefinition" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.ir.value.Codec.decodeDefinition(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg4 <- c.downN(4)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                    } yield morphir.ir.Value.LetDefinition(
                      arg1,
                      arg2,
                      arg3,
                      arg4
                    )
                  case "LetRecursion" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.sdk.dict.Codec.decodeDict(
                                    morphir.ir.name.Codec.decodeName,
                                    morphir.ir.value.Codec.decodeDefinition(
                                      decodeTa,
                                      decodeVa
                                    )
                                  )
                                )
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                    } yield morphir.ir.Value.LetRecursion(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "List" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.sdk.list.Codec.decodeList(
                                    morphir.ir.value.Codec.decodeValue(
                                      decodeTa,
                                      decodeVa
                                    )
                                  )
                                )
                    } yield morphir.ir.Value.List(
                      arg1,
                      arg2
                    )
                  case "Literal" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.literal.Codec.decodeLiteral)
                    } yield morphir.ir.Value.Literal(
                      arg1,
                      arg2
                    )
                  case "PatternMatch" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.sdk.list.Codec.decodeList(
                                    (
                                      (c: io.circe.HCursor) =>
                                        for {
                                          arg1 <- c.downN(0).as(morphir.ir.value.Codec.decodePattern(decodeVa))
                                          arg2 <- c.downN(1).as(morphir.ir.value.Codec.decodeValue(
                                                    decodeTa,
                                                    decodeVa
                                                  ))
                                        } yield (arg1, arg2)
                                    )
                                  )
                                )
                    } yield morphir.ir.Value.PatternMatch(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "Record" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.sdk.dict.Codec.decodeDict(
                                    morphir.ir.name.Codec.decodeName,
                                    morphir.ir.value.Codec.decodeValue(
                                      decodeTa,
                                      decodeVa
                                    )
                                  )
                                )
                    } yield morphir.ir.Value.Record(
                      arg1,
                      arg2
                    )
                  case "Reference" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
                    } yield morphir.ir.Value.Reference(
                      arg1,
                      arg2
                    )
                  case "Tuple" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.sdk.list.Codec.decodeList(
                                    morphir.ir.value.Codec.decodeValue(
                                      decodeTa,
                                      decodeVa
                                    )
                                  )
                                )
                    } yield morphir.ir.Value.Tuple(
                      arg1,
                      arg2
                    )
                  case "Unit" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                    } yield morphir.ir.Value.Unit(arg1)
                  case "UpdateRecord" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2)
                                .as(
                                  morphir.ir.value.Codec.decodeValue(
                                    decodeTa,
                                    decodeVa
                                  )
                                )
                      arg3 <- c.downN(3)
                                .as(
                                  morphir.sdk.dict.Codec.decodeDict(
                                    morphir.ir.name.Codec.decodeName,
                                    morphir.ir.value.Codec.decodeValue(
                                      decodeTa,
                                      decodeVa
                                    )
                                  )
                                )
                    } yield morphir.ir.Value.UpdateRecord(
                      arg1,
                      arg2,
                      arg3
                    )
                  case "Variable" =>
                    for {
                      arg1 <- c.downN(1).as(decodeVa)
                      arg2 <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
                    } yield morphir.ir.Value.Variable(
                      arg1,
                      arg2
                    )
                }
            )
          )
    )

}
