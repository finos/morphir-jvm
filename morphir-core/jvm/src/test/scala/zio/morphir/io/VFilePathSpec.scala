package zio.morphir.io

import zio.test._
object VFilePathSpec extends ZIOSpecDefault {
  def spec = suite("VFilePath") {
    implicit val fileSeparator = FileSeparator("/")
    val windowsStyleSeparator  = FileSeparator("\\")
    test("Creation") {
      val root: VFilePath     = VFilePath.root
      val absolute: VFilePath = VFilePath("/foo/bar/baz")
      val relative: VFilePath = VFilePath("foo/bar/baz")
      assertTrue(
        root.toString == "/",
        root.path == root.toString(),
        root.isAbsolute == true,
        absolute.toString() == "/foo/bar/baz",
        absolute.path == absolute.toString(),
        absolute == VFilePath.AbsolutePath("foo", "bar", "baz"),
        absolute.isAbsolute == true,
        relative.toString() == "foo/bar/baz",
        relative.path == relative.toString(),
        relative.isAbsolute == false,
        root != absolute,
        root != relative,
        absolute != relative
      )
    } + test("dirname") {
      val root: VFilePath     = VFilePath.root
      val absolute: VFilePath = VFilePath("/alpha/beta/gamma/delta")
      val oneLevelRelative    = VFilePath("child")
      assertTrue(
        root.dirname == root,
        absolute.dirname == VFilePath("/alpha/beta/gamma"),
        oneLevelRelative.dirname == VFilePath(".")
      )
    } + test("concatenation with '/'") {
      val root     = VFilePath.root
      val winRoot  = VFilePath.root(windowsStyleSeparator)
      val relative = VFilePath("home/user/foo/bar")
      assertTrue(
        root / winRoot == winRoot,
        root / relative == VFilePath.AbsolutePath("home", "user", "foo", "bar"),
        (root / relative).path == "/home/user/foo/bar"
      )
    }
  }
}
