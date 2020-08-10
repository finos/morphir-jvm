/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
            """["private",["John","Doe",35]]"""
          )
        },
        test("A Public object should encode as expected") {
          assertEncodesToExpectedCompactJsonString(AccessControlled.`public`(("John", "Doe", 35)))(
            """["public",["John","Doe",35]]"""
          )
        }
      ),
      suite("Decoding from JSON")(
        test(
          "Given valid JSON text for a public item it should decode successfully"
        )(
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "public"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin))(equalTo(Good(publicAccess(("John", 31)))))
        ),
        test(
          "Given valid JSON text for a private item it should decode successfully"
        ) {
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "private"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin))(equalTo(Good(privateAccess(("John", 31)))))
        },
        test(
          "Given an invalid $type tag in the JSON decoding should fail"
        ) {
          assert(decodeString[AccessControlled[(String, Int)]]("""
                                                                 |[ "protected"
                                                                 |, ["John", 31]    
                                                                 |]
              """.stripMargin).isGood)(isFalse)
        }
      )
    )
  )
}
