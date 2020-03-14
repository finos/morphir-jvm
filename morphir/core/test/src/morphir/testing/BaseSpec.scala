package morphir.testing

import upickle.default._
import zio.Tagged
import zio.test._
import zio.test.Assertion._
import zio.test.environment._

trait BaseSpec { this: DefaultRunnableSpec =>

  def testEncodesToJSON[T](sut: T, json: String)(
      implicit encoder: Writer[T],
      tag: Tagged[T]
  ) = {
    val typeName = tag.tag.shortName
    test(s"Given $typeName: $sut it should encode to: $json") {
      assert(write(sut))(equalTo(json))
    }
  }
}
