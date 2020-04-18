package org.morphir.ir

import zio.test._
//import scala.language.implicitConversions
import morphir.testing.BaseSpec

import Name.name
import QName.qName
object QNameSpec extends DefaultRunnableSpec with BaseSpec {

  def spec = suite("QNameSpec")(
    suite("Encoding/Decoding a QName")(
      testEncodesToJSON(
        qName(morphirIRModulePath, name("name")),
        """[[["morphir"],["i","r"]],["name"]]"""
      ),
      testEncodesToJSON(
        qName(morphirIRModulePath, name("q", "name")),
        """[[["morphir"],["i","r"]],["q","name"]]"""
      ),
      testEncodesToJSON(
        qName(morphirIRAdvancedModulePath, name("type")),
        """[[["morphir"],["i","r"],["advanced"]],["type"]]"""
      )
    )
  )

  val morphirIRModulePath =
    Path.fromNames(
      name("morphir"),
      name("i", "r")
    )

  val morphirIRAdvancedModulePath =
    Path.fromNames(
      name("morphir"),
      name("i", "r"),
      name("advanced")
    )
}
