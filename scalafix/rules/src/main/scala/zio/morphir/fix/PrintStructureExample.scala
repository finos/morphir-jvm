package zio.morphir.fix

import scalafix.v1.{Patch, SyntacticDocument, SyntacticRule}
import scala.meta.*

object PrintStructureExample extends App {
  // import zio.morphir.module

  val struct = q"""  
  import zio.morphir.module 

  @module(namespace = Some("Morphir.SDK")) object Bool {}"""
  val src = Source(List(Pkg(Term.Name("zio.morphir.fix"), List(struct))))
  println(src.syntax)
}