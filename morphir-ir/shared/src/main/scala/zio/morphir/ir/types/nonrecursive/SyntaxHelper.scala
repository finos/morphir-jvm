package zio.morphir.ir.types.nonrecursive

import zio.Chunk
import zio.morphir.ir.types.nonrecursive.Type._

object SyntaxHelper {
  final class DefineFunction[Attributes](val paramTypes: () => Chunk[Type[Attributes]]) extends AnyVal {
    final def apply(returnType: Type[Attributes], attributes: Attributes): Type[Attributes] =
      Function(attributes, paramTypes(), returnType)
  }
}
