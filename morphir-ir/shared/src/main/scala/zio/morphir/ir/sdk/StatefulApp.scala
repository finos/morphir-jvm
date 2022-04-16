package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module
import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.Type.Constructors
import zio.morphir.ir.Type.Specification.CustomTypeSpecification
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.sdk.Maybe.maybeType
import zio.morphir.syntax.NamingSyntax._

object StatefulApp {
  val moduleName: ModuleName = ModuleName.fromString("StatefulApp")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("StatefulApp") -> CustomTypeSpecification(
        Chunk(name("k"), name("c"), name("s"), name("e")),
        Constructors(
          Map(
            name("StatefulApp") -> Chunk(
              (
                name("logic"),
                function(
                  maybeType(variable(name("s"))),
                  function(
                    variable(name("c")),
                    tuple(
                      Chunk(
                        maybeType(variable(name("s"))),
                        variable(name("e"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) ?? "Type that represents a stateful app."
    ),
    values = Map.empty
  )
}
