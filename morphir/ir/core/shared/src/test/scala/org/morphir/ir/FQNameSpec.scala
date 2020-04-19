package org.morphir.ir

import zio.test._
import morphir.testing.BaseSpec

import Name.name
import Path.path
import FQName.fQName

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
