package zio.morphir.ir.sdk

import zio.morphir.ir.Module
import zio.morphir.ir.Module.{ModuleName}

object Basics {
  val moduleName: ModuleName            = ModuleName.fromString("Basics")
  val moduleSpec: Module.USpecification = Module.USpecification(types = ???, values = ???)
}
