import org.apache.spark.sql.SparkSession
import org.scalatest.FunSuite

class test extends FunSuite {
  val localTestSession = SparkSession.builder().master("local").appName("Example").getOrCreate()
  test("test initializing spark context") {

    val list = List(1, 2, 3, 4)
    val rdd  = localTestSession.sparkContext.parallelize(list)

    assert(rdd.count === list.length)
    localTestSession.close()

  }
  test("test that dataframe is created") {}
  test("test that filter result is correct") {}
}
