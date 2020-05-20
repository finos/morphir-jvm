package morphir.ir

import morphir.ir.json.JsonFacade
import zio.test._
import zio.test.Assertion._

object ExprKindSpec extends DefaultRunnableSpec with JsonFacade {
  def spec = suite("ExprKind Spec")(
    suite("TypeExprKind Spec")(
      suite("Misc...")(
        test("isTypeExpr should return true for all cases")(
          assert(TypeExprKind.valuesSet)(forall[TypeExprKind](assertion("isTypeExpr")()(_.isTypeExpr)))
        )
      ),
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
        ),
        suite("Decoding")(
          TypeExprKind.values.map { kind =>
            test(s"""Decoding $kind should decode from a JSON String of: "$kind" """)(
              assert(decodeString[TypeExprKind](s""""$kind"""").toEither)(isRight(equalTo(kind)))
            )
          }: _*
        )
      )
    ),
    suite("ValueExprKind Spec")()
  )
}
