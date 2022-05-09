import com.github.mrpowers.spark.fast.tests.DatasetComparer
import org.apache.spark
import org.apache.spark.sql.types.StringType
import utest.{TestSuite, Tests}

object ExampleTests extends TestSuite with SparkSessionTestWrapper with DatasetComparer {

  val tests = Tests {

    import spark.implicits._

    "withGreeting" - {

      val sourceDF = spark.createDF(
        List(
          ("ebuka"),
          ("osborne"),
          ("elorm")
        ), List(
          ("name", StringType, true)
        )
      )

      val actualDF = sourceDF.transform(Example.withGreeting())

      val expectedDF = Seq(
        ("ebuka", "good morning !"),
        ("osborne", "good morning !"),
        ("elorm", "good morning !")
      ).toDF("name", "greeting")

      assertSmallDatasetEquality(actualDF, expectedDF, ignoreNullable = true)

    }

  }

}