package morphir.ir.codec

import morphir.ir.path.Path
import morphir.ir.Name
import zio.test._
import zio.test.Assertion._
import morphir.ir.json.JsonFacade

object ModuleCodecSpec extends DefaultRunnableSpec with JsonFacade {
  def spec = suite("ModuleCodec Spec")(
    suite("ModulePath JSON")(
      suite("Encoding")(
        test("A ModulePath should encode to the same JSON as a regular path") {
          val path = Path.fromNames(
            Name.name("alpha", "omega"),
            Name.name("beta", "delta"),
            Name.name("gamma")
          )

          val modulePath = path.toModulePath

          assert(encodeAsJson(modulePath))(equalTo(encodeAsJson(path)))

        }
      ),
      suite("Decoding")()
    )
  )
}
