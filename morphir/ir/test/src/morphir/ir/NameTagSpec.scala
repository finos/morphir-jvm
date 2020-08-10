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

import zio.test._
import zio.test.Assertion._

object NameTagSpec extends DefaultRunnableSpec {
  def spec = suite("NameTag Spec")(
    suite("Creation")(
      test("It should be creatable from a String")(
        assert(NameTag.fromString[TestNode]("NameTag"))(
          hasField("tag", (nt: NameTag[TestNode]) => nt.tag, equalTo("name_tag")) && hasField(
            "name",
            (nt: NameTag[TestNode]) => nt.name,
            equalTo(Name.fromString("NameTag"))
          )
        )
      ),
      test("It should be creatable just given a type that has a Tag")(
        assert(NameTag.of[TestNode])(
          hasField("tag", (nt: NameTag[TestNode]) => nt.tag, equalTo("test_node")) && hasField(
            "name",
            (nt: NameTag[TestNode]) => nt.name,
            equalTo(Name.fromString("TestNode"))
          )
        )
      )
    ),
    test("It should be creatable just given a type that has a ClassTag")(
      assert(NameTag.forClass[TestNode])(
        hasField("tag", (nt: NameTag[TestNode]) => nt.tag, equalTo("test_node")) && hasField(
          "name",
          (nt: NameTag[TestNode]) => nt.name,
          equalTo(Name.fromString("TestNode"))
        )
      )
    ),
    test("It should be creatable when given a parameterized type that has a ClassTag")(
      assert(NameTag.forClass[ParameterizedTestNode[String]])(
        hasField("tag", (nt: NameTag[ParameterizedTestNode[String]]) => nt.tag, equalTo("parameterized_test_node")) && hasField(
          "name",
          (nt: NameTag[ParameterizedTestNode[String]]) => nt.name,
          equalTo(Name.fromString("ParameterizedTestNode"))
        )
      )
    )
  )

  case class TestNode()
  case class ParameterizedTestNode[A]()
}
