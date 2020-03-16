package morphir.ir

import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import morphir.testing.BaseSpec

import Name.name
import Path.path
import QName.qName
import FQName.fQName

import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._

object FQNameSpec extends DefaultRunnableSpec with BaseSpec {
  def spec = suite("FQNameSpec")(
    suite("Encoding and decoding")(
      testEncodesToJSON(
        fQName(
          path(name("morphir"), name("core")),
          path(name("morphir"), name("i", "r")),
          name("f", "q", "name")
        ),
        """[[["morphir"],["core"]],[["morphir"],["i","r"]],["f","q","name"]]"""
      )
    )
  )
}
