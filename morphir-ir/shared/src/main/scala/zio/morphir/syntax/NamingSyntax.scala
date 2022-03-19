package zio.morphir.syntax

import zio.morphir.ir.Name

trait NamingSyntax {
  def name(name: String): Name = Name.fromString(name)
}

object NamingSyntax extends NamingSyntax
