package morphir.sdk

import zio.test._
import zio.test.Assertion._
import zio.{ console, ZIO }
import morphir.sdk.spark.testing.SparkSpec
import morphir.sdk.spark.testing.sparkModule
object SdkTypesEncodingSpec extends SparkSpec {
  def spec = suite("SdkTypesEncoding Spec")(
    testM("Encoding should work for a row with a Decimal")(
      for {
        data    <- ZIO.succeed(List(MyRow(MyDecimal(BigDecimal(1.1))), MyRow(MyDecimal(BigDecimal(3.14)))))
        dataset <- sparkModule.createDataset(data)
        actual <- sparkModule { _ =>
                    dataset.collect().toList
                  }
        _ <- console.putStrLn(s"Actual: $actual")
      } yield assert(actual)(equalTo(data))
    )
  )

  //import zio.prelude._

  //object MyDecimal extends Subtype[scala.BigDecimal]
  //type MyDecimal = MyDecimal.Type

  object MyDecimal {
    def apply(value: BigDecimal): MyDecimal = value
  }
  type MyDecimal = BigDecimal

  final case class MyRow(amount: MyDecimal)
}
