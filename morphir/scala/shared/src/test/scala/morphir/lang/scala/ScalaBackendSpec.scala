package morphir.lang.scala

import morphir.ir.Type
import morphir.ir.Name
import morphir.ir.sdk
import zio.test._
import zio.test.Assertion._
import _root_.scala.meta._

object ScalaBackendSpec extends DefaultRunnableSpec {
  def spec = suite("ScalaBackend Spec")(
    suite("Generating Scala")(
      suite("For a Record")(
        test("A simple record should generate a corresponding case class") {
          import Type._

          val recordType = record(
            field(Name.fromString("firstName"), sdk.String.stringType),
            field(Name.fromString("lastName"), sdk.String.stringType),
            field(Name.fromString("yearOfBirth"), sdk.Int.intType)
          )

          val typeDef = Name.fromString("Contact") -> recordType

          val sut = ScalaBackend.Live()

          val result = ??? //sut.generate(recordType)
          assert(result)(equalTo(q"case class Foo()"))
        }
      )
    )
  )
}
