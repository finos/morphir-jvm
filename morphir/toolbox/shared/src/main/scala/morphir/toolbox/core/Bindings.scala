package morphir.toolbox.core

import zio.ZIO

final case class Bindings(value: Map[String, Binding]) extends AnyVal {
  def getAsEffect(name: String): ZIO[Any, Errors.NoSuchBinding, Binding] =
    ZIO.fromOption(value.get(name)).orElseFail(Errors.NoSuchBinding(name))
}
object Bindings {}
