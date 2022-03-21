package zio.morphir.json

import zio.json._
import zio.morphir.ir._
import zio.morphir.ir.AccessControlled.Access._
import zio.morphir.ir.Literal
import zio.morphir.ir.TypeModule._

object Decoders {
  trait MorphirJsonCodecV1 {
    implicit val unitDecoder: JsonDecoder[Unit] = JsonDecoder.list[String].mapOrFail {
      case a if a.isEmpty => Right(())
      case a              => Left(s"Expected empty list, got [${a.mkString(", ")}]")
    }
    implicit val nameDecoder: JsonDecoder[Name]               = JsonDecoder.list[String].map(Name.fromList)
    implicit val pathDecoder: JsonDecoder[Path]               = JsonDecoder.list[Name].map(Path.fromList)
    implicit val modulePathDecoder: JsonDecoder[ModulePath]   = pathDecoder.map(ModulePath(_))
    implicit val packageNameDecoder: JsonDecoder[PackageName] = pathDecoder.map(PackageName(_))
    implicit val qNameDecoder: JsonDecoder[QName]             = JsonDecoder.tuple2[Path, Name].map(QName.fromTuple)
    implicit val fqNameDecoder: JsonDecoder[FQName] =
      JsonDecoder.tuple3[PackageName, ModulePath, Name].map(x => FQName(x._1, x._2, x._3))

    implicit val moduleNameDecoder: JsonDecoder[ModuleModule.ModuleName] =
      JsonDecoder.tuple2[Path, Name].map(x => ModuleModule.ModuleName(x._1, x._2))

    implicit def literalBoolDecoder: JsonDecoder[Literal.Bool] =
      JsonDecoder.tuple2[String, Boolean].map(x => Literal.Bool(x._2))

    implicit def literalCharDecoder: JsonDecoder[Literal.Char] =
      JsonDecoder.tuple2[String, Char].map(x => Literal.Char(x._2))

    implicit def literalFloatDecoder: JsonDecoder[Literal.Float] =
      JsonDecoder.tuple2[String, java.math.BigDecimal].map(x => Literal.Float(x._2))

    implicit def literalStringDecoder: JsonDecoder[Literal.String] =
      JsonDecoder.tuple2[String, String].map(x => Literal.String(x._2))

    implicit def literalWholeNumberDecoder: JsonDecoder[Literal.WholeNumber] =
      JsonDecoder.tuple2[String, java.math.BigInteger].map(x => Literal.WholeNumber(x._2))

    implicit def fieldDecoder[A](implicit decoder: JsonDecoder[A]): JsonDecoder[Field[A]] =
      JsonDecoder.tuple2[Name, A].map(x => Field(x._1, x._2))

    implicit def documentedDecoder[A](implicit valueDecoder: JsonDecoder[A]): JsonDecoder[Documented[A]] =
      JsonDecoder.tuple2[String, A].map(x => Documented(x._1, x._2))

    implicit def accessControlledDecoder[A](implicit encoder: JsonDecoder[A]): JsonDecoder[AccessControlled[A]] = {
      JsonDecoder.tuple2[String, A].map { x =>
        AccessControlled(
          x._1 match {
            case "public"  => Public
            case "private" => Private
          },
          x._2
        )
      }
    }
  }

  object MorphirJsonCodecV1 extends MorphirJsonCodecV1
}
