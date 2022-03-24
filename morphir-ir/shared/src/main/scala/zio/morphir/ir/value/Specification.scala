package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.TypeModule.Type

final case class Specification[+TA](inputs: Chunk[(Name, Type[TA])], output: Type[TA]) { self =>
  def map[B](f: TA => B): Specification[B] =
    Specification(inputs.map { case (name, tpe) => (name, tpe.mapAttributes(f)) }, output.mapAttributes(f))
}

object Specification {
  def create[Attributes](inputs: (Name, Type[Attributes])*): Inputs[Attributes] =
    new Inputs(() => Chunk.fromIterable(inputs))

  final class Inputs[Annotations](private val inputs: () => Chunk[(Name, Type[Annotations])]) extends AnyVal {
    def apply(output: Type[Annotations]): Specification[Annotations] =
      Specification(inputs(), output)
  }
}
