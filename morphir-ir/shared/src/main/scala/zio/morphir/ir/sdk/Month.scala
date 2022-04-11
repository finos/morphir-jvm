package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module
import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.Type.Specification.CustomTypeSpecification
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.Type.{Type, UConstructors, UType}
import zio.morphir.ir.sdk.Common._
import zio.morphir.syntax.NamingSyntax._

object Month {
  val moduleName: ModuleName = ModuleName.fromString("Month")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Month")
        -> CustomTypeSpecification(
          Chunk.empty,
          UConstructors(
            Map(
              name("January")   -> Chunk.empty,
              name("February")  -> Chunk.empty,
              name("March")     -> Chunk.empty,
              name("April")     -> Chunk.empty,
              name("May")       -> Chunk.empty,
              name("June")      -> Chunk.empty,
              name("July")      -> Chunk.empty,
              name("August")    -> Chunk.empty,
              name("September") -> Chunk.empty,
              name("October")   -> Chunk.empty,
              name("November")  -> Chunk.empty,
              name("December")  -> Chunk.empty
            )
          )
        )
        ?? "Type that represents an month concept."
    ),
    values = Map.empty
  )

  lazy val dateType: UType                = reference(toFQName(moduleName, "Month"))
  def dateType[A](attributes: A): Type[A] = reference(attributes, toFQName(moduleName, "Month"))
}
