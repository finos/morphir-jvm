package morphir.flowz.spark

import morphir.flowz.spark.testing.SparkSpec
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object SparkSupportSpec extends SparkSpec with SparkSupport {
  def spec = suite("SparkSupport Spec")(
    suite("Using splitWith")(
      testM("It should be possible to split a Dataset of product types into 2 parts using splitWith")(
        for {
          dataset <- sparkModule.createDataset(Seq.range(1, 11).map(Item(_)))
          results <- ZIO.succeed(dataset.splitWith {
                       case n if n.value % 2 == 0 => Right(RightThing(n.value)): Either[LeftThing[Int], RightThing[Int]]
                       case n => Left(LeftThing(n.value))
                     })
        } yield {
          val (lhs, rhs) = (results._1.collect().toList, results._2.collect().toList)
          assert(lhs)(hasSize(equalTo(5))) && assert(rhs)(hasSize(equalTo(5)))
        }
      )
    ),
    suite("Using splitWithCodec")(
      testM("It should be possible to split a Dataset of encodable types into 2 parts using splitWithCodec")(
        for {
          dataset <- sparkModule.makeDataset { spark =>
                       import spark.implicits._
                       spark.createDataset(Seq.range(1, 11))
                     }
          results <- ZIO.succeed(dataset.splitWith {
                       case n if n % 2 == 0 => Right(RightThing(n)): Either[LeftThing[Int], RightThing[Int]]
                       case n => Left(LeftThing(n))
                     })
        } yield {
          val (lhs, rhs) = (results._1.collect().toList, results._2.collect().toList)
          assert(lhs)(hasSize(equalTo(5))) && assert(rhs)(hasSize(equalTo(5)))
        }
      )
    )
  )
  final case class Item[A](value: A)
  final case class LeftThing[A](value: A)
  final case class RightThing[A](value: A)
}
