package zio.morphir

import scala.annotation.Annotation

/**
 * An annotation that indicates that the annotated type is a Morphir module.
 *
 * @param nameOverride
 *   An optional name override for the module. If not specified, the name of the annotated type is used.
 */
final case class module(
    nameOverride: Option[String] = None
) extends Annotation

/**
 * An annotation that indicates that the annotated item should appear in the provided namespace.
 * @param ns
 *   The namespace to use.
 */
final case class namespace(ns: String) extends Annotation

/**
 * An annotation that indicates that the annotated item should appear as being contained in the named distribution
 * package.
 * @param name
 * @param sinceVersion
 * @param untilVersion
 */
final case class distributionPackage(
    name: String,
    sinceVersion: Option[String] = None,
    untilVersion: Option[String] = None
) extends Annotation

/**
 * An annotation that indicates that the annotated item is a native function.
 */
final case class nativeFunction() extends Annotation
