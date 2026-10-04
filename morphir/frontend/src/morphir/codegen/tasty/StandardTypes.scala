package morphir.codegen.tasty

import morphir.ir.Type.Type
import morphir.ir.{ FQName, Type as MorphType }
import morphir.sdk.List as MorphList

object StandardTypes {

  val boolReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("basics")("bool")
    MorphType.Reference((), fQName, MorphList.empty[MorphType.Type[Unit]])
  }

  val intReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("basics")("int")
    MorphType.Reference((), fQName, MorphList.empty[MorphType.Type[Unit]])
  }

  val floatReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("basics")("float")
    MorphType.Reference((), fQName, MorphList.empty[MorphType.Type[Unit]])
  }

  val stringReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("string")("string")
    MorphType.Reference((), fQName, MorphList.empty[MorphType.Type[Unit]])
  }

  val charReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("char")("char")
    MorphType.Reference((), fQName, MorphList.empty[MorphType.Type[Unit]])
  }

  val decimalReference: Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("decimal")("decimal")
    MorphType.Reference((), fQName, MorphList.empty[MorphType.Type[Unit]])
  }

  val unitType: MorphType.Type[Unit] =
    MorphType.Unit(())

  def maybeReference(types: MorphList.List[MorphType.Type[Unit]]): Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("maybe")("maybe")
    MorphType.Reference((), fQName, types)
  }

  def listReference(types: MorphList.List[MorphType.Type[Unit]]): Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("list")("list")
    MorphType.Reference((), fQName, types)
  }

  def dictReference(types: MorphList.List[MorphType.Type[Unit]]): Type.Reference[Unit] = {
    val fQName = FQName.fqn("morphir.SDK")("dict")("dict")
    MorphType.Reference((), fQName, types)
  }
}
