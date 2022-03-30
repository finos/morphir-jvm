package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.{Documented, Module, Name}
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.ValueModule.Specification
import zio.morphir.syntax.NamingSyntax._
import zio.morphir.ir.Type.Type.unit

object Regex {
  val moduleName: ModuleName = ModuleName.fromString("Regex")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map.empty,
    values = toSpec(
      Chunk(
        "fromString",
        "fromStringWith",
        "never",
        "contains",
        "split",
        "find",
        "replace",
        "splitAtMost",
        "findAtMost",
        "replaceAtMost"
      )
    )
  )

  private def toSpec(values: Chunk[String]): Map[Name, Documented[Specification[Any]]] =
    values.map(valueName => (name(valueName), Documented("", Specification(Chunk.empty, unit)))).toMap
}
