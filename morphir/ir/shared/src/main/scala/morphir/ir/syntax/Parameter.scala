package morphir.ir.syntax

import morphir.ir.{ Name, Parameter, Type }

import scala.language.implicitConversions

trait ParameterSyntax {
  final implicit def morphirSyntaxParameter[A](parameter: Parameter[A]): ParameterOps[A] =
    new ParameterOps[A](parameter)
}

object ParameterSyntax extends ParameterSyntax

final class ParameterOps[A](val self: Parameter[A]) extends AnyVal {
  @inline def parameterName: Name    = self._1
  @inline def parameterType: Type[A] = self._2

  def mapAttributes[B](f: A => B): Parameter[B] = parameterName -> parameterType.mapAttributes(f)
}
