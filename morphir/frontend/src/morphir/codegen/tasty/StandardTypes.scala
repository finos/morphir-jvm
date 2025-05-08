package morphir.codegen.tasty

import morphir.ir.Type.Type
import morphir.ir.{FQName, Type as MorphType}

object StandardTypes {

  val intReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("basics")("int")
    MorphType.Reference((), fQName, List.empty[MorphType.Type[Unit]])
  }
  
  val floatReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("basics")("float")
    MorphType.Reference((), fQName, List.empty[MorphType.Type[Unit]])
  }
  
  val stringReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("string")("string")
    MorphType.Reference((), fQName, List.empty[MorphType.Type[Unit]])
  }
}
