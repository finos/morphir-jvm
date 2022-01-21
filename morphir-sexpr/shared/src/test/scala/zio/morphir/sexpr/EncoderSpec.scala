package zio.morphir.sexpr

import zio.morphir.testing.ZioBaseSpec
import zio.test._

object EncoderSpec extends ZioBaseSpec {
  def spec = suite("Encoder")(
    suite("toSExpr")(
      suite("primitives")(
        test("string") {
          assertTrue(
            "hello world".toSExpr == "\"hello world\"",
            "hello\nworld".toSExpr == "\"hello\\nworld\"",
            "hello\rworld".toSExpr == "\"hello\\rworld\"",
            "hello\u0000world".toSExpr == "\"hello\\u0000world\""
          )
        } + test("boolean") {
          assertTrue(true.toSExpr == "true", false.toSExpr == "false")
        } + test("byte") {
          assertTrue(1.toSExpr == "1")
        } + test("char") {
          assertTrue(
            'c'.toSExpr == "\"c\""
            // Symbol("c").toSExpr == "\"c\""
          )
        } + test("float") {
          assertTrue(
            Float.NaN.toSExpr == "\"NaN\"",
            Float.PositiveInfinity.toSExpr == "\"Infinity\"",
            Float.NegativeInfinity.toSExpr == "\"-Infinity\"",
            0.0f.toSExpr == "0.0",
            (-0.0f).toSExpr == "-0.0",
            1.0f.toSExpr == "1.0",
            (-1.0f).toSExpr == "-1.0",
            1.0e7f.toSExpr == "1.0E7",
            (-1.0e7f).toSExpr == "-1.0E7",
            1.17549435e-38f.toSExpr == "1.1754944E-38",
            9999999.0f.toSExpr == "9999999.0",
            0.001f.toSExpr == "0.001",
            9.999999e-4f.toSExpr == "9.999999E-4",
            1.4e-45f.toSExpr == "1.4E-45",
            3.3554448e7f.toSExpr == "3.355445E7",
            8.999999e9f.toSExpr == "9.0E9",
            3.4366718e10f.toSExpr == "3.436672E10",
            4.7223665e21f.toSExpr == "4.7223665E21",
            8388608.0f.toSExpr == "8388608.0",
            1.6777216e7f.toSExpr == "1.6777216E7",
            3.3554436e7f.toSExpr == "3.3554436E7",
            6.7131496e7f.toSExpr == "6.7131496E7",
            1.9310392e-38f.toSExpr == "1.9310392E-38",
            (-2.47e-43f).toSExpr == "-2.47E-43",
            1.993244e-38f.toSExpr == "1.993244E-38",
            4103.9004f.toSExpr == "4103.9004",
            5.3399997e9f.toSExpr == "5.3399997E9",
            6.0898e-39f.toSExpr == "6.0898E-39",
            0.0010310042f.toSExpr == "0.0010310042",
            2.8823261e17f.toSExpr == "2.882326E17",
            7.038531e-26f.toSExpr == "7.038531E-26",
            9.2234038e17f.toSExpr == "9.223404E17",
            6.7108872e7f.toSExpr == "6.710887E7",
            1.0e-44f.toSExpr == "9.8E-45",
            2.816025e14f.toSExpr == "2.816025E14",
            9.223372e18f.toSExpr == "9.223372E18",
            1.5846086e29f.toSExpr == "1.5846086E29",
            1.1811161e19f.toSExpr == "1.1811161E19",
            5.368709e18f.toSExpr == "5.368709E18",
            4.6143166e18f.toSExpr == "4.6143166E18",
            0.007812537f.toSExpr == "0.007812537",
            1.4e-45f.toSExpr == "1.4E-45",
            1.18697725e20f.toSExpr == "1.18697725E20",
            1.00014165e-36f.toSExpr == "1.00014165E-36",
            200.0f.toSExpr == "200.0",
            3.3554432e7f.toSExpr == "3.3554432E7",
            1.26217745e-29f.toSExpr == "1.2621775E-29",
            1.0e-43f.toSExpr == "9.9E-44",
            1.0e-45f.toSExpr == "1.4E-45",
            7.1e10f.toSExpr == "7.1E10",
            1.1e15f.toSExpr == "1.1E15",
            1.0e17f.toSExpr == "1.0E17",
            6.3e9f.toSExpr == "6.3E9",
            3.0e10f.toSExpr == "3.0E10",
            1.1e10f.toSExpr == "1.1E10",
            (-6.9390464e7f).toSExpr == "-6.939046E7",
            (-6939.0464f).toSExpr == "-6939.0464"
          )
        } + test("double") {
          assertTrue(
            Double.NaN.toSExpr == "\"NaN\"",
            Double.PositiveInfinity.toSExpr == "\"Infinity\"",
            Double.NegativeInfinity.toSExpr == "\"-Infinity\"",
            0.0d.toSExpr == "0.0",
            (-0.0d).toSExpr == "-0.0",
            1.0d.toSExpr == "1.0",
            (-1.0d).toSExpr == "-1.0",
            2.2250738585072014e-308d.toSExpr == "2.2250738585072014E-308",
            1.0e7d.toSExpr == "1.0E7",
            (-1.0e7d).toSExpr == "-1.0E7",
            9999999.999999998d.toSExpr == "9999999.999999998",
            0.001d.toSExpr == "0.001",
            9.999999999999998e-4d.toSExpr == "9.999999999999998E-4",
            (-1.7976931348623157e308d).toSExpr == "-1.7976931348623157E308",
            4.9e-324d.toSExpr == "4.9E-324",
            1.7976931348623157e308d.toSExpr == "1.7976931348623157E308",
            (-2.109808898695963e16d).toSExpr == "-2.109808898695963E16",
            4.940656e-318d.toSExpr == "4.940656E-318",
            1.18575755e-316d.toSExpr == "1.18575755E-316",
            2.989102097996e-312d.toSExpr == "2.989102097996E-312",
            9.0608011534336e15d.toSExpr == "9.0608011534336E15",
            4.708356024711512e18d.toSExpr == "4.708356024711512E18",
            9.409340012568248e18d.toSExpr == "9.409340012568248E18",
            1.8531501765868567e21d.toSExpr == "1.8531501765868567E21",
            (-3.347727380279489e33d).toSExpr == "-3.347727380279489E33",
            1.9430376160308388e16d.toSExpr == "1.9430376160308388E16",
            (-6.9741824662760956e19d).toSExpr == "-6.9741824662760956E19",
            4.3816050601147837e18d.toSExpr == "4.3816050601147837E18",
            7.1202363472230444e-307d.toSExpr == "7.120236347223045E-307",
            3.67301024534615e16d.toSExpr == "3.67301024534615E16",
            5.9604644775390625e-8d.toSExpr == "5.960464477539063E-8",
            1.0e-322d.toSExpr == "9.9E-323",
            5.0e-324d.toSExpr == "4.9E-324",
            1.0e23d.toSExpr == "1.0E23",
            8.41e21d.toSExpr == "8.41E21",
            7.3879e20d.toSExpr == "7.3879E20",
            3.1e22d.toSExpr == "3.1E22",
            5.63e21d.toSExpr == "5.63E21",
            2.82879384806159e17d.toSExpr == "2.82879384806159E17",
            1.387364135037754e18d.toSExpr == "1.387364135037754E18",
            1.45800632428665e17d.toSExpr == "1.45800632428665E17",
            1.790086667993e18d.toSExpr == "1.790086667993E18",
            2.273317134858e18d.toSExpr == "2.273317134858E18",
            7.68905065813e17d.toSExpr == "7.68905065813E17",
            1.9400994884341945e25d.toSExpr == "1.9400994884341945E25",
            3.6131332396758635e25d.toSExpr == "3.6131332396758635E25",
            2.5138990223946153e25d.toSExpr == "2.5138990223946153E25",
            (-3.644554028000364e16d).toSExpr == "-3.644554028000364E16",
            (-6939.0464d).toSExpr == "-6939.0464"
          )
        } + test("int") {
          assertTrue(1.toSExpr == "1")
        }
      )
    ),
    suite("toSExprAst")()
  )
}
