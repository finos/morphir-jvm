package zio.morphir.syntax

import zio.ZEnvironment
import zio.morphir.ir.{Literal => Lit, ValueModule}
import ValueModule.Value

trait ValueSyntax {
  import Value.*

  final def boolean(value: Boolean): Literal[Boolean, Any] = Literal(Lit.boolean(value), ZEnvironment.empty)
  final def boolean[Annotations](
      value: Boolean,
      annotations: ZEnvironment[Annotations]
  ): Literal[Boolean, Annotations] =
    Literal(Lit.boolean(value), annotations)

  final def literal[V](value: Lit[V]): Literal[V, Any] = Literal(value, ZEnvironment.empty)
  final def literal[V, Annotations](value: Lit[V], annotations: ZEnvironment[Annotations]): Literal[V, Annotations] =
    Literal(value, annotations)

  final def string[Annotations](value: String, annotations: ZEnvironment[Annotations]): Value[Annotations] =
    Literal(Lit.string(value), annotations)

  final val unit: Unit[Any]                                                              = Unit(ZEnvironment.empty)
  final def unit[Annotations](annotations: ZEnvironment[Annotations]): Unit[Annotations] = Unit(annotations)
}
