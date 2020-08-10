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


//package morphir.lang.scala
//
//import morphir.ir.{ Type => TypeExpr }
//import morphir.ir.Name.name
//import morphir.ir.sdk
//import zio.test._
//import zio.test.Assertion._
////import zio.test.TestAspect._
//import _root_.scala.meta._
//
//object ScalaBackendSpec extends DefaultRunnableSpec {
//  def spec = suite("ScalaBackend Spec")(
//    suite("Generating a Scala Tree")(
//      suite("For a Record")(
//        test("A simple record should generate a corresponding case class") {
//          import TypeExpr._
//
//          val expected =
//            q"""case class Contact(
//               firstName:morphir.sdk.string.String,
//               lastName:morphir.sdk.string.String,
//               yearOfBirth:morphir.sdk.string.Int
//               )"""
//
//          val recordType = record(
//            field(name("firstName"), sdk.String.stringType),
//            field(name("lastName"), sdk.String.stringType),
//            field(name("yearOfBirth"), sdk.Int.intType)
//          )
//
//          val sut = ScalaBackend.Live()
//
//          val actual = sut.toTree(name("Contact"))(recordType)
//          assert(actual.syntax)(equalTo(expected.syntax))
//        },
//        test("A parametric record type should generate a corresponding case class") {
//          import TypeExpr._
//
//          val expected = q"""case class Foo[A](value:A)"""
//
//          val recordType = record(
//            field(name("value"), TypeExpr.Variable({}, name("a")))
//          )
//
//          val sut = ScalaBackend.Live()
//
//          val actual = sut.toTree(name("Foo"))(recordType)
//          assert(actual.syntax)(equalTo(expected.syntax))
//        }
//      )
//    )
//  )
//
//  def treesAreEqual[A <: Tree](expected: A): Assertion[A] = {
//    import _root_.scala.meta.contrib._
//    assertion("treeIsEqualTo")()(actual => actual.isEqual(expected))
//  }
//}
