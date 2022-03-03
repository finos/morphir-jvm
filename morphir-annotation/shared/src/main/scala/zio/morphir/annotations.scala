package zio.morphir

import scala.annotation.Annotation

final case class module(
    name: Option[String] = None,
    namespace: Option[String] = None,
    distributionName: Option[String] = None
) extends Annotation

final case class nativeFunction() extends Annotation
