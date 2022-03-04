package zio.morphir.json
import zio.json._
import zio.json.internal.Write
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.ValueModule.ValueCase.UnitCase
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.ir.{ModuleDefinition, Name, PackageDefinition}

trait MorphirJsonCodecV1 {

  // NOTE: We will want to create JSON encoders which follow the format in the morphir-elm project
  implicit val unitEncoder: JsonEncoder[Unit] = JsonEncoder.list[String].contramap(_ => List.empty[String])
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)

  implicit def valueEncoder[Annotations](implicit
      annotationsEncoder: JsonEncoder[Annotations]
  ): JsonEncoder[Value[Annotations]] = ???
  implicit def typeEncoder[Annotations](implicit
      annotationsEncoder: JsonEncoder[Annotations]
  ): JsonEncoder[Type[Annotations]] = ???

  def encodePackageDefinition[Annotations](
      packageDefinition: PackageDefinition[Annotations]
  )(implicit annotationEncoder: JsonEncoder[Annotations]): CharSequence = {
    // TODO: Look here for hints at implementing: https://github.com/finos/morphir-elm/blob/9122b99a6f500af389bfa26c2f7c604ea64485c2/src/Morphir/IR/Package/CodecV1.elm
    ???
  }

  def encodeModuleDefinition[Annotations](moduleDef: ModuleDefinition[Annotations])(implicit
      annotationEncoder: JsonEncoder[Annotations]
  ): CharSequence = ???

}

object MorphirJsonCodecV1 extends MorphirJsonCodecV1
