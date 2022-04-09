package zio.morphir.samples

import zio.ZEnvironment
import zio.morphir.ir.Type.UType

object AttributionExample {

  sealed trait ValueAttribute

  object ValueAttribute {

    final case class AscribedType(tpe: UType) extends ValueAttribute
    // final case class Attribute1(value: File with Closeable) extends Attribute
    // final case class Attribute2(value: Int) extends Attribute

    def mapAttributes(environment: ZEnvironment[ValueAttribute])(
        f: ValueAttribute => ValueAttribute
    ): ZEnvironment[ValueAttribute] =
      environment.update(f)

  }
}
