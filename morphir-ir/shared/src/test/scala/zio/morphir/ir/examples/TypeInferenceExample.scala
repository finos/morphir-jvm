package zio.morphir.ir.examples

import zio.morphir.ir.FQName
import zio.morphir.ir.types.UType
import zio.morphir.ir.value.{RawValue, Value}

object TypeInferenceExample {

  import zio.Chunk
  import Value.{Unit => UnitType}
  import zio.morphir.ir.Name
  import zio.morphir.ir.{Literal => Lit}

  implicit final class AddTypeSyntax[TA](private val self: Value[TA, Unit]) extends AnyVal {
    def setType[TA](uType: UType): Value[TA, UType] = ???
  }

  import zio.morphir.ir.types.Type

  val intType: UType    = Type.Reference((), FQName.fromString("Package:Module:Int"), Chunk.empty)
  val stringType: UType = Type.Reference((), FQName.fromString("Package:Module:String"), Chunk.empty)
  val personType: UType = ???

  val u: RawValue = UnitType()
  u.setType(intType)

  val myRecord = Value.Record(
    Chunk(
      Name.fromString("name") -> Value.Literal(Lit.string("adam")),
      Name.fromString("age")  -> Value.Literal(Lit.int(32))
    )
  )

  val myTypedRecord = Value
    .Record(
      personType,
      Chunk(
        Name.fromString("name") -> Value.Literal(stringType, Lit.string("adam")),
        Name.fromString("age")  -> Value.Literal(intType, Lit.int(32))
      )
    )
    .mapAttributes(identity(_), tpe => tpe ?? "Docs")

//  type EmptyCaps[A] = Unit
//  type X = ZEnvironmentSubset[Any]

//  def inferTypes[TA](value: Value[TA,Any]): Value[TA, UType] = ???

  trait AttributeTransformer[Attributes] {
    def apply(attributes: UType): Attributes
  }

  final case class AttributeApplier(initialAttributes: UType)

//  trait Value {
//    def @@(attributeTransformer: AttributeTransformer): Value
//  }
  // myRecord.inferTypes

  final case class Annotations(annotations: Map[String, Any])

  trait ZIO[-R, +E, +A]

}

object TreeDefinition {
  import zio.Tag
  import zio.morphir.ir.ZEnvironmentSubset

  final case class Tree[Capabilities[_], +Value, +Annotations](
      value: Value,
      annotations: ZEnvironmentSubset[Capabilities, Annotations]
  ) { self =>
    def zipWith[Value2, Value3, Annotations1 >: Annotations](that: Tree[Capabilities, Value2, Annotations1])(
        f: (Value, Value2) => Value3
    )(implicit tag: Tag[Annotations1]): Tree[Capabilities, Value3, Annotations1] =
      Tree(f(self.value, that.value), self.annotations.union[Annotations1](that.annotations))
  }

  // val dummyTree:Tree[]
}

object Default {
  import zio._

  val defaultEnvironment: ZEnvironment[Clock with Console with Random with System] = ZEnvironment.default

  val test: ZEnvironment[Clock with Console with Random] = defaultEnvironment

  val test2: ZEnvironment[Any] = defaultEnvironment
}

object Serialization {
  import TreeDefinition._

  trait Json

  trait JsonCodec[A] {
    def encode(a: A): Json
    def decode(json: Json): A
  }

  def encodeTree[Value, Annotations](tree: Tree[JsonCodec, Value, Annotations]): Json =
    ???
}

object Usage {
  import Serialization._
  trait MyAnnotation

  object MyAnnotation {
    implicit val MyAnnotationCodec: JsonCodec[MyAnnotation] = ???
  }

}
