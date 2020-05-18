//package morphir.lang.scala
//
//import morphir.ir.{FQName, Name, Path, Type}
//import zio.test._
//import zio.test.Assertion._
//
//object ScalaBackendSpec extends DefaultRunnableSpec {
//  def spec = suite("ScalaBackend Spec")(
//    suite("Generating Scala")(
//      suite("For a Record")(
//        test("A simple record should generate a corresponding case class") {
//          import Type._
//          import Name.name
//          import FQName.fQName
//          import Path.path
//          val recordType = record(
//            //field(name("firstName"), Type.Reference(fQName(Path.fromString("org.morphir.sdk"),path("String"),name(String))))
//          )
//      )
//    )
//  )
//}