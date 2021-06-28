package org.finos.morphir.ir

/**
 * Type that represents a value or function specification.
 * The specification of what the value or function is without the actual
 * data or logic behind it.
 */
final case class ValueSpecification[+A](inputs: List[(Name, Type[A])], output: Type[A]):
  def transform[B](f:A => B):ValueSpecification[B] =
      ValueSpecification(
          inputs = inputs.map { case (name, tpe) => (name, tpe.transform(f))},
          output = output.transform(f)
      )

