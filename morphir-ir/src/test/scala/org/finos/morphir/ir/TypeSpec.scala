package org.finos.morphir.ir
import zio.test.{assert, DefaultRunnableSpec}
import zio.test.Assertion.*
object TypeSpec extends DefaultRunnableSpec:
    def spec = suite("TypeSpec")(
        test("Types with different annotations should not be equal"){
            val typeA = Type("A", TypeDetails.Unit)
            val typeB = Type("B", TypeDetails.Unit)
            assert(typeA)(not (equalTo(typeB)))
        },
        test("unit constant should be a Type[Unit]"){
            val expected:Type[Unit] = Type.unit(())
            val actual = Type.unit
            assert(actual)(equalTo(expected))
        },
        test("annotateWith should map the attributes directly on the Type"){
            val expected = Type.unit("Test!")
            val actual = Type.unit.transform(_ => "Test!")
            assert(actual)(equalTo(expected))
        }
    )