package org.morphir.ir

import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import morphir.testing.BaseSpec

import AccessControlled._

import upickle.default._

object AccessControlledSpec extends DefaultRunnableSpec with BaseSpec {
  def spec = suite("AccessControlledSpec")(
    suite("JSON Encoding/Decoding")(
      suite("Encoding to JSON")(
        test("A Private object should encode as expected") {
          val sut = AccessControlled.`private`(("John", "Doe", 35))
          assert(write(sut))(
            equalTo("""{"$type":"private","value":["John","Doe",35]}""")
          )
        },
        test("A Public object should encode as expected") {
          val sut = AccessControlled.`public`(("John", "Doe", 35))
          assert(write(sut))(
            equalTo("""{"$type":"public","value":["John","Doe",35]}""")
          )
        }
      ),
      suite("Decoding from JSON")(
        test(
          "Given valid JSON text for a public item it should decode successfully"
        ) {
          checkDecodesFromJSON(
            """
              |{ "$type": "public"
              |, "value": ["John", 31]    
              |}
              """.stripMargin,
            publicAccess(("John", 31))
          )
        },
        test(
          "Given valid JSON text for a private item it should decode successfully"
        ) {
          checkDecodesFromJSON(
            """
              |{ "$type": "private"
              |, "value": ["John", 31]    
              |}
              """.stripMargin,
            privateAccess(("John", 31))
          )
        },
        test(
          "Given an invalid $type tag in the JSON decoding should fail"
        ) {
          checkDecodesFromJSON(
            """
              |{ "$type": "protected"
              |, "value": ["John", "Smith", 31]    
              |}
              """.stripMargin,
            privateAccess(("John", "Smith", 31))
          )
        } @@ failure
      )
    )
  )
}
