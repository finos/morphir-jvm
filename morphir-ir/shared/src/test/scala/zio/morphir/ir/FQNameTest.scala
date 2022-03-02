package zio.morphir.ir

import zio.morphir.testing.MorphirBaseSpec
import zio.test.*

object FQNameTest extends MorphirBaseSpec {
  def spec = suite("FQName")(
    suite("Create a FQName:")(
      test("By using a string") {
        assertTrue(
          FQName.fromString("moduleName/packageName/localName", "/") ==
            FQName(
              PackageName(Path.fromString("moduleName")),
              ModulePath(Path.fromString("packageName")),
              Name.fromString("localName")
            )
        )
      },
      test("By using a QName") {
        val path  = Path.fromString("package Name")
        val qName = QName(Path.fromString("qualified.Name.Path"), Name.fromString("localName"))
        assertTrue(FQName.fromQName(path, qName) == FQName(path, qName.modulePath, qName.localName))
      }
    ),
    suite("Retrieving variables should work")(
      test("Get PackagePath") {
        val fqName = FQName.fromString("moduleName/packageName/localName", "/")
        assertTrue(FQName.getPackagePath(fqName) == Path.fromString("moduleName"))
      },
      test("Get ModulePath") {
        val fqName = FQName.fromString("moduleName/packageName/localName", "/")
        assertTrue(FQName.getModulePath(fqName) == Path.fromString("packageName"))
      },
      test("Get localName") {
        val fqName = FQName.fromString("moduleName/packageName/localName", "/")
        assertTrue(FQName.getLocalName(fqName) == Name.fromString("localName"))
      }
    ),
    suite("Creating a string from FQName") {
      test("should work") {
        assertTrue(
          FQName.toString(
            FQName(
              PackageName(Path.fromString("com.example")),
              ModulePath(Path.fromString("java home")),
              Name.fromString("morphir")
            )
          ) == "Com.Example:JavaHome:morphir"
        )
      }
    }
  )
}
