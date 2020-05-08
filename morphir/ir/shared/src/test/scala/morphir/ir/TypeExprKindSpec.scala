package morphir.ir

import morphir.ir.json.JsonFacade
import zio.test._
import zio.test.Assertion._

object TypeExprKindSpec extends DefaultRunnableSpec with JsonFacade {
  def spec = suite("TypeExprKind Spec")(
    suite("When listing values")(
      test("Should include all objects") {
        assert(TypeExprKind.values.toSet)(
          equalTo(
            Set(
              TypeExprKind.Variable,
              TypeExprKind.Reference,
              TypeExprKind.Tuple,
              TypeExprKind.Record,
              TypeExprKind.ExtensibleRecord,
              TypeExprKind.Function,
              TypeExprKind.Unit
            )
          )
        )
      }
    ),
    suite("Name to Value mapping")(
      test("The companion object should provide a namesToValuesMap")(
        assert(TypeExprKind.namesToValuesMap)(
          equalTo(
            TypeExprKind.values.map(v => v.entryName -> v).toMap
          )
        )
      )
    ),
    suite("toString")(
      test("ToString of entries should return their name")(
        assert(TypeExprKind.values.map(_.toString).toSet)(
          equalTo(Set("Variable", "Reference", "Tuple", "Record", "ExtensibleRecord", "Function", "Unit"))
        )
      )
    ),
    suite("JSON Specs")(
      suite("Encoding")(
        TypeExprKind.values.map { kind =>
          test(s"""Encoding $kind should encode as the JSON String: "$kind" """)(
            assert(encode(kind, 0))(equalTo(s""""$kind""""))
          )
        }: _*
      )
    )
  )
}
