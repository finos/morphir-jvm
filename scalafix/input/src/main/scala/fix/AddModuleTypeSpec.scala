/*
rule = AddModuleTypeSpec
 */
package zio.morphir.sdk
import zio.morphir._

@module @namespace("Morphir.SDK") object Bool {}

@module(Some("Basics")) @namespace("Morphir.SDK") object BasicsModule {}

package foo.bar {
  object Foo
}
