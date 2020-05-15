package morphir.ir

import cats.data.Validated._
import morphir.ir.AccessControlled._
import morphir.ir.json.JsonFacade
import morphir.ir.testing.JsonSpec
import zio.test.Assertion._
import zio.test._

object AccessControlledSpec extends DefaultRunnableSpec with JsonSpec with JsonFacade {
  def spec = suite("AccessControlledSpec")(
    suite("JSON Encoding/Decoding")(
      suite("Encoding to JSON")(
        test("A Private object should encode as expected") {
          val sut = AccessControlled.`private`(("John", "Doe", 35))
          assert(encode(sut, 0))(
            equalTo("""["Private",["John","Doe",35]]""")
          )
        },
        test("A Public object should encode as expected") {
          val sut = AccessControlled.`public`(("John", "Doe", 35))
          assert(encode(sut, 0))(
            equalTo("""["Public",["John","Doe",35]]""")
          )
        }
      ),
      suite("Decoding from JSON")(
        test(
          "Given valid JSON text for a public item it should decode successfully"
        )(
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "Public"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin))(equalTo(Valid(publicAccess(("John", 31)))))
        ),
        test(
          "Given valid JSON text for a private item it should decode successfully"
        ) {
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "Private"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin))(equalTo(Valid(privateAccess(("John", 31)))))
        },
        test(
          "Given an invalid $type tag in the JSON decoding should fail"
        ) {
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "Protected"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin).isValid)(isFalse)
        }
      )
    )
  )
}
