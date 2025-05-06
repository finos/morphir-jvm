package morphir.ir.value

/** Generated based on IR.Value
 */
object Codec{

  implicit def encodeDefinition[Ta, Va](
                                         encodeTa: io.circe.Encoder[Ta],
                                         encodeVa: io.circe.Encoder[Va]
                                       ): io.circe.Encoder[morphir.ir.Value.Definition[Ta, Va]] =
    ((definition: morphir.ir.Value.Definition[Ta, Va]) =>
      io.circe.Json.obj(
        ("""inputTypes""", morphir.sdk.list.Codec.encodeList(((definition: (morphir.ir.Name.Name, Va, morphir.ir.Type.Type[Ta])) =>
          io.circe.Json.arr(
            morphir.ir.name.Codec.encodeName(definition._1),
            encodeVa(definition._2),
            morphir.ir._type.Codec.encodeType(encodeTa)(definition._3)
          )))(definition.inputTypes)),
        ("""outputType""", morphir.ir._type.Codec.encodeType(encodeTa)(definition.outputType)),
        ("""body""", morphir.ir.value.Codec.encodeValue(
          encodeTa,
          encodeVa
        )(definition.body))
      ))

  implicit def encodePattern[A](
                                 encodeA: io.circe.Encoder[A]
                               ): io.circe.Encoder[morphir.ir.Value.Pattern[A]] =
    ((pattern: morphir.ir.Value.Pattern[A]) =>
      pattern match {
        case morphir.ir.Value.AsPattern(attrs, pattern, name) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""AsPattern"""),
            encodeA(attrs),
            morphir.ir.value.Codec.encodePattern(encodeA)(pattern),
            morphir.ir.name.Codec.encodeName(name)
          )
        case morphir.ir.Value.ConstructorPattern(attrs, constructorName, argumentPatterns) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""ConstructorPattern"""),
            encodeA(attrs),
            morphir.ir.fqname.Codec.encodeFQName(constructorName),
            morphir.sdk.list.Codec.encodeList(morphir.ir.value.Codec.encodePattern(encodeA))(argumentPatterns)
          )
        case morphir.ir.Value.EmptyListPattern(attrs) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""EmptyListPattern"""),
            encodeA(attrs)
          )
        case morphir.ir.Value.HeadTailPattern(attrs, headPattern, tailPattern) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""HeadTailPattern"""),
            encodeA(attrs),
            morphir.ir.value.Codec.encodePattern(encodeA)(headPattern),
            morphir.ir.value.Codec.encodePattern(encodeA)(tailPattern)
          )
        case morphir.ir.Value.LiteralPattern(attrs, value) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""LiteralPattern"""),
            encodeA(attrs),
            morphir.ir.literal.Codec.encodeLiteral(value)
          )
        case morphir.ir.Value.TuplePattern(attrs, elementPatterns) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""TuplePattern"""),
            encodeA(attrs),
            morphir.sdk.list.Codec.encodeList(morphir.ir.value.Codec.encodePattern(encodeA))(elementPatterns)
          )
        case morphir.ir.Value.UnitPattern(attrs) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""UnitPattern"""),
            encodeA(attrs)
          )
        case morphir.ir.Value.WildcardPattern(attrs) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""WildcardPattern"""),
            encodeA(attrs)
          )
      })

  implicit val encodeRawValue: io.circe.Encoder[morphir.ir.Value.RawValue] = morphir.ir.value.Codec.encodeValue(
    morphir.sdk.basics.Codec.encodeUnit,
    morphir.sdk.basics.Codec.encodeUnit
  )

  implicit def encodeSpecification[Ta](
                                        encodeTa: io.circe.Encoder[Ta]
                                      ): io.circe.Encoder[morphir.ir.Value.Specification[Ta]] =
    ((specification: morphir.ir.Value.Specification[Ta]) =>
      io.circe.Json.obj(
        ("""inputs""", morphir.sdk.list.Codec.encodeList(((specification: (morphir.ir.Name.Name, morphir.ir.Type.Type[Ta])) =>
          io.circe.Json.arr(
            morphir.ir.name.Codec.encodeName(specification._1),
            morphir.ir._type.Codec.encodeType(encodeTa)(specification._2)
          )))(specification.inputs)),
        ("""output""", morphir.ir._type.Codec.encodeType(encodeTa)(specification.output))
      ))

  implicit val encodeTypedValue: io.circe.Encoder[morphir.ir.Value.TypedValue] = morphir.ir.value.Codec.encodeValue(
    morphir.sdk.basics.Codec.encodeUnit,
    morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
  )

  implicit def encodeValue[Ta, Va](
                                    encodeTa: io.circe.Encoder[Ta],
                                    encodeVa: io.circe.Encoder[Va]
                                  ): io.circe.Encoder[morphir.ir.Value.Value[Ta, Va]] =
    ((value: morphir.ir.Value.Value[Ta, Va]) =>
      value match {
        case morphir.ir.Value.Apply(attrs, func, argument) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Apply"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(func),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(argument)
          )
        case morphir.ir.Value.Constructor(attrs, fullyQualifiedName) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Constructor"""),
            encodeVa(attrs),
            morphir.ir.fqname.Codec.encodeFQName(fullyQualifiedName)
          )
        case morphir.ir.Value.Destructure(attrs, pattern, valueToDestruct, inValue) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Destructure"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodePattern(encodeVa)(pattern),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(valueToDestruct),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(inValue)
          )
        case morphir.ir.Value.Field(attrs, subjectValue, fieldName) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Field"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(subjectValue),
            morphir.ir.name.Codec.encodeName(fieldName)
          )
        case morphir.ir.Value.FieldFunction(attrs, fieldName) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""FieldFunction"""),
            encodeVa(attrs),
            morphir.ir.name.Codec.encodeName(fieldName)
          )
        case morphir.ir.Value.IfThenElse(attrs, condition, thenBranch, elseBranch) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""IfThenElse"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(condition),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(thenBranch),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(elseBranch)
          )
        case morphir.ir.Value.Lambda(attrs, argumentPattern, body) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Lambda"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodePattern(encodeVa)(argumentPattern),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(body)
          )
        case morphir.ir.Value.LetDefinition(attrs, valueName, valueDefinition, inValue) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""LetDefinition"""),
            encodeVa(attrs),
            morphir.ir.name.Codec.encodeName(valueName),
            morphir.ir.value.Codec.encodeDefinition(
              encodeTa,
              encodeVa
            )(valueDefinition),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(inValue)
          )
        case morphir.ir.Value.LetRecursion(attrs, valueDefinitions, inValue) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""LetRecursion"""),
            encodeVa(attrs),
            morphir.sdk.dict.Codec.encodeDict(
              morphir.ir.name.Codec.encodeName,
              morphir.ir.value.Codec.encodeDefinition(
                encodeTa,
                encodeVa
              )
            )(valueDefinitions),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(inValue)
          )
        case morphir.ir.Value.List(attrs, items) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""List"""),
            encodeVa(attrs),
            morphir.sdk.list.Codec.encodeList(morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            ))(items)
          )
        case morphir.ir.Value.Literal(attrs, value) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Literal"""),
            encodeVa(attrs),
            morphir.ir.literal.Codec.encodeLiteral(value)
          )
        case morphir.ir.Value.PatternMatch(attrs, branchOutOn, cases) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""PatternMatch"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(branchOutOn),
            morphir.sdk.list.Codec.encodeList(((cases: (morphir.ir.Value.Pattern[Va], morphir.ir.Value.Value[Ta, Va])) =>
              io.circe.Json.arr(
                morphir.ir.value.Codec.encodePattern(encodeVa)(cases._1),
                morphir.ir.value.Codec.encodeValue(
                  encodeTa,
                  encodeVa
                )(cases._2)
              )))(cases)
          )
        case morphir.ir.Value.Record(attrs, fields) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Record"""),
            encodeVa(attrs),
            morphir.sdk.dict.Codec.encodeDict(
              morphir.ir.name.Codec.encodeName,
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )
            )(fields)
          )
        case morphir.ir.Value.Reference(attrs, fullyQualifiedName) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Reference"""),
            encodeVa(attrs),
            morphir.ir.fqname.Codec.encodeFQName(fullyQualifiedName)
          )
        case morphir.ir.Value.Tuple(attrs, elements) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Tuple"""),
            encodeVa(attrs),
            morphir.sdk.list.Codec.encodeList(morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            ))(elements)
          )
        case morphir.ir.Value.Unit(attrs) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Unit"""),
            encodeVa(attrs)
          )
        case morphir.ir.Value.UpdateRecord(attrs, valueToUpdate, fieldsToUpdate) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""UpdateRecord"""),
            encodeVa(attrs),
            morphir.ir.value.Codec.encodeValue(
              encodeTa,
              encodeVa
            )(valueToUpdate),
            morphir.sdk.dict.Codec.encodeDict(
              morphir.ir.name.Codec.encodeName,
              morphir.ir.value.Codec.encodeValue(
                encodeTa,
                encodeVa
              )
            )(fieldsToUpdate)
          )
        case morphir.ir.Value.Variable(attrs, name) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("""Variable"""),
            encodeVa(attrs),
            morphir.ir.name.Codec.encodeName(name)
          )
      })

  implicit def decodeDefinition[Ta, Va](
                                         decodeTa: io.circe.Decoder[Ta],
                                         decodeVa: io.circe.Decoder[Va]
                                       ): io.circe.Decoder[morphir.ir.Value.Definition[Ta, Va]] =
    ((c: io.circe.HCursor) =>
      for {
        inputTypes_ <- c.downField("""inputTypes""").as(morphir.sdk.list.Codec.decodeList(((c: io.circe.HCursor) =>
          for {
            arg1 <- c.downN(0).as(morphir.ir.name.Codec.decodeName)
            arg2 <- c.downN(1).as(decodeVa)
            arg3 <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeTa))
          }  yield (arg1, arg2, arg3))))
        outputType_ <- c.downField("""outputType""").as(morphir.ir._type.Codec.decodeType(decodeTa))
        body_ <- c.downField("""body""").as(morphir.ir.value.Codec.decodeValue(
          decodeTa,
          decodeVa
        ))
      }  yield morphir.ir.Value.Definition(
        inputTypes_,
        outputType_,
        body_
      ))

  implicit def decodePattern[A](
                                 decodeA: io.circe.Decoder[A]
                               ): io.circe.Decoder[morphir.ir.Value.Pattern[A]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case """AsPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
              pattern <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeA))
              name <- c.downN(3).as(morphir.ir.name.Codec.decodeName)
            }  yield morphir.ir.Value.AsPattern(
              attrs,
              pattern,
              name
            )
          case """ConstructorPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
              constructorName <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
              argumentPatterns <- c.downN(3).as(morphir.sdk.list.Codec.decodeList(morphir.ir.value.Codec.decodePattern(decodeA)))
            }  yield morphir.ir.Value.ConstructorPattern(
              attrs,
              constructorName,
              argumentPatterns
            )
          case """EmptyListPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
            }  yield morphir.ir.Value.EmptyListPattern(attrs)
          case """HeadTailPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
              headPattern <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeA))
              tailPattern <- c.downN(3).as(morphir.ir.value.Codec.decodePattern(decodeA))
            }  yield morphir.ir.Value.HeadTailPattern(
              attrs,
              headPattern,
              tailPattern
            )
          case """LiteralPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
              value <- c.downN(2).as(morphir.ir.literal.Codec.decodeLiteral)
            }  yield morphir.ir.Value.LiteralPattern(
              attrs,
              value
            )
          case """TuplePattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
              elementPatterns <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir.value.Codec.decodePattern(decodeA)))
            }  yield morphir.ir.Value.TuplePattern(
              attrs,
              elementPatterns
            )
          case """UnitPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
            }  yield morphir.ir.Value.UnitPattern(attrs)
          case """WildcardPattern""" =>
            for {
              attrs <- c.downN(1).as(decodeA)
            }  yield morphir.ir.Value.WildcardPattern(attrs)
        })))

  implicit val decodeRawValue: io.circe.Decoder[morphir.ir.Value.RawValue] = morphir.ir.value.Codec.decodeValue(
    morphir.sdk.basics.Codec.decodeUnit,
    morphir.sdk.basics.Codec.decodeUnit
  )

  implicit def decodeSpecification[Ta](
                                        decodeTa: io.circe.Decoder[Ta]
                                      ): io.circe.Decoder[morphir.ir.Value.Specification[Ta]] =
    ((c: io.circe.HCursor) =>
      for {
        inputs_ <- c.downField("""inputs""").as(morphir.sdk.list.Codec.decodeList(((c: io.circe.HCursor) =>
          for {
            arg1 <- c.downN(0).as(morphir.ir.name.Codec.decodeName)
            arg2 <- c.downN(1).as(morphir.ir._type.Codec.decodeType(decodeTa))
          }  yield (arg1, arg2))))
        output_ <- c.downField("""output""").as(morphir.ir._type.Codec.decodeType(decodeTa))
      }  yield morphir.ir.Value.Specification(
        inputs_,
        output_
      ))

  implicit val decodeTypedValue: io.circe.Decoder[morphir.ir.Value.TypedValue] = morphir.ir.value.Codec.decodeValue(
    morphir.sdk.basics.Codec.decodeUnit,
    morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
  )

  implicit def decodeValue[Ta, Va](
                                    decodeTa: io.circe.Decoder[Ta],
                                    decodeVa: io.circe.Decoder[Va]
                                  ): io.circe.Decoder[morphir.ir.Value.Value[Ta, Va]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case """Apply""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              func <- c.downN(2).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              argument <- c.downN(3).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
            }  yield morphir.ir.Value.Apply(
              attrs,
              func,
              argument
            )
          case """Constructor""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              fullyQualifiedName <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
            }  yield morphir.ir.Value.Constructor(
              attrs,
              fullyQualifiedName
            )
          case """Destructure""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              pattern <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeVa))
              valueToDestruct <- c.downN(3).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              inValue <- c.downN(4).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
            }  yield morphir.ir.Value.Destructure(
              attrs,
              pattern,
              valueToDestruct,
              inValue
            )
          case """Field""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              subjectValue <- c.downN(2).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              fieldName <- c.downN(3).as(morphir.ir.name.Codec.decodeName)
            }  yield morphir.ir.Value.Field(
              attrs,
              subjectValue,
              fieldName
            )
          case """FieldFunction""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              fieldName <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
            }  yield morphir.ir.Value.FieldFunction(
              attrs,
              fieldName
            )
          case """IfThenElse""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              condition <- c.downN(2).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              thenBranch <- c.downN(3).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              elseBranch <- c.downN(4).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
            }  yield morphir.ir.Value.IfThenElse(
              attrs,
              condition,
              thenBranch,
              elseBranch
            )
          case """Lambda""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              argumentPattern <- c.downN(2).as(morphir.ir.value.Codec.decodePattern(decodeVa))
              body <- c.downN(3).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
            }  yield morphir.ir.Value.Lambda(
              attrs,
              argumentPattern,
              body
            )
          case """LetDefinition""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              valueName <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
              valueDefinition <- c.downN(3).as(morphir.ir.value.Codec.decodeDefinition(
                decodeTa,
                decodeVa
              ))
              inValue <- c.downN(4).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
            }  yield morphir.ir.Value.LetDefinition(
              attrs,
              valueName,
              valueDefinition,
              inValue
            )
          case """LetRecursion""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              valueDefinitions <- c.downN(2).as(morphir.sdk.dict.Codec.decodeDict(
                morphir.ir.name.Codec.decodeName,
                morphir.ir.value.Codec.decodeDefinition(
                  decodeTa,
                  decodeVa
                )
              ))
              inValue <- c.downN(3).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
            }  yield morphir.ir.Value.LetRecursion(
              attrs,
              valueDefinitions,
              inValue
            )
          case """List""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              items <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              )))
            }  yield morphir.ir.Value.List(
              attrs,
              items
            )
          case """Literal""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              value <- c.downN(2).as(morphir.ir.literal.Codec.decodeLiteral)
            }  yield morphir.ir.Value.Literal(
              attrs,
              value
            )
          case """PatternMatch""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              branchOutOn <- c.downN(2).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              cases <- c.downN(3).as(morphir.sdk.list.Codec.decodeList(((c: io.circe.HCursor) =>
                for {
                  arg1 <- c.downN(0).as(morphir.ir.value.Codec.decodePattern(decodeVa))
                  arg2 <- c.downN(1).as(morphir.ir.value.Codec.decodeValue(
                    decodeTa,
                    decodeVa
                  ))
                }  yield (arg1, arg2))))
            }  yield morphir.ir.Value.PatternMatch(
              attrs,
              branchOutOn,
              cases
            )
          case """Record""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              fields <- c.downN(2).as(morphir.sdk.dict.Codec.decodeDict(
                morphir.ir.name.Codec.decodeName,
                morphir.ir.value.Codec.decodeValue(
                  decodeTa,
                  decodeVa
                )
              ))
            }  yield morphir.ir.Value.Record(
              attrs,
              fields
            )
          case """Reference""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              fullyQualifiedName <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
            }  yield morphir.ir.Value.Reference(
              attrs,
              fullyQualifiedName
            )
          case """Tuple""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              elements <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              )))
            }  yield morphir.ir.Value.Tuple(
              attrs,
              elements
            )
          case """Unit""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
            }  yield morphir.ir.Value.Unit(attrs)
          case """UpdateRecord""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              valueToUpdate <- c.downN(2).as(morphir.ir.value.Codec.decodeValue(
                decodeTa,
                decodeVa
              ))
              fieldsToUpdate <- c.downN(3).as(morphir.sdk.dict.Codec.decodeDict(
                morphir.ir.name.Codec.decodeName,
                morphir.ir.value.Codec.decodeValue(
                  decodeTa,
                  decodeVa
                )
              ))
            }  yield morphir.ir.Value.UpdateRecord(
              attrs,
              valueToUpdate,
              fieldsToUpdate
            )
          case """Variable""" =>
            for {
              attrs <- c.downN(1).as(decodeVa)
              name <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
            }  yield morphir.ir.Value.Variable(
              attrs,
              name
            )
        })))

}