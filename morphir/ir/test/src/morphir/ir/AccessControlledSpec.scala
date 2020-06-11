package morphir.ir

import morphir.ir.AccessControlled._
import morphir.ir.testing.JsonSpec
import morphir.ir.json.JsonFacade
import org.scalactic.Good
import zio.test.Assertion._
import zio.test._

object AccessControlledSpec extends DefaultRunnableSpec with JsonSpec with JsonFacade {
  def spec = suite("AccessControlledSpec")(
    suite("JSON Encoding/Decoding")(
      suite("Encoding to JSON")(
        test("A Private object should encode as expected") {
          assertEncodesToExpectedCompactJsonString(AccessControlled.`private`(("John", "Doe", 35)))(
            """["Private",["John","Doe",35]]"""
          )
        },
        test("A Public object should encode as expected") {
          assertEncodesToExpectedCompactJsonString(AccessControlled.`public`(("John", "Doe", 35)))(
            """["Public",["John","Doe",35]]"""
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
              """.stripMargin))(equalTo(Good(publicAccess(("John", 31)))))
        ),
        test(
          "Given valid JSON text for a private item it should decode successfully"
        ) {
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "Private"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin))(equalTo(Good(privateAccess(("John", 31)))))
        },
        test(
          "Given an invalid $type tag in the JSON decoding should fail"
        ) {
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "Protected"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin).isGood)(isFalse)
        }
      )
    )
  )
}
