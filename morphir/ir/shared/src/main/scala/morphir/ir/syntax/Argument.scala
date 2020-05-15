package morphir.ir.syntax

import morphir.ir.{ Argument, Name }

import scala.language.implicitConversions

trait ArgumentSyntax {
  final implicit def morphirSyntaxArgumentOps[A](argument: Argument[A]): ArgumentOps[A] = new ArgumentOps[A](argument)
}

object ArgumentSyntax extends ArgumentSyntax

final class ArgumentOps[+A](val self: Argument[A]) extends AnyVal {
  @inline def argName: Name = self._1
  @inline def attributes: A = self._2

  def mapAttributes[B](f: A => B): Argument[B]       = Argument(argName, f(attributes))
  def map[B](f: (Name, A) => (Name, B)): Argument[B] = f(argName, attributes)
}
