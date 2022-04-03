package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Name
import zio.morphir.ir.types.Type

final case class Specification[+TA](inputs: Chunk[(Name, Type[TA])], output: Type[TA]) { self =>
  def map[B](f: TA => B): Specification[B] =
    Specification(
      inputs = inputs.map { case (name, tpe) => (name, tpe.mapAttributes(f)) },
      output = output.mapAttributes(f)
    )
}

object Specification {
  def create[Attributes](inputs: (Name, Type[Attributes])*): Inputs[Attributes] =
    new Inputs(() => Chunk.fromIterable(inputs))

  type Raw = Specification[Unit]
  object Raw {
    def apply(inputs: (String, Type[Unit])*)(output: Type[Unit]): Raw =
      Specification(inputs = Chunk.fromIterable(inputs.map { case (n, t) => Name.fromString(n) -> t }), output = output)
  }

  final class Inputs[Annotations](private val inputs: () => Chunk[(Name, Type[Annotations])]) extends AnyVal {
    def apply(output: Type[Annotations]): Specification[Annotations] =
      Specification(inputs(), output)
  }
}
