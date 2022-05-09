import org.apache.spark.sql.SparkSession
import org.scalatest.FunSuite

class test extends FunSuite {
  test("test initailizing spark context"){
    val myLocalInProcessSession = SparkSession.builder().master("local").appName("Example").getOrCreate()

    val list = List(1,2,3,4)
    val rdd = myLocalInProcessSession.sparkContext.parallelize(list)

    assert(rdd.count === list.length)
    myLocalInProcessSession.close()

  }
}