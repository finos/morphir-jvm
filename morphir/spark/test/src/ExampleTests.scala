import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.explode
import org.scalatest.FunSuite

case class School(id: String, name: String)
case class Student(firstName: String, lastname: String, email: String, age: Int)
case class SchoolWithStudents(school: School, students: Seq[Student])

class test extends FunSuite {
  val localTestSession = SparkSession.builder().master("local").appName("Example").getOrCreate()
  import localTestSession.implicits._
  test("test initializing spark context") {

    val list = List(1, 2, 3, 4)
    val rdd  = localTestSession.sparkContext.parallelize(list)

    assert(rdd.count === list.length)
    localTestSession.close()

  }
  test("First colunm should be John") {
    val school                = School("1234", "Eaton Square")
    val student               = Student("John", "Clark", "john@mail.com", 18)
    val schoolWithStudents    = SchoolWithStudents(school, Seq(student))
    val schoolWithStudentsSeq = Seq(schoolWithStudents)
    val df1                   = schoolWithStudentsSeq.toDF()
    val explodeDF             = df1.select(explode($"students"))

    val flattenDF = explodeDF.select($"col.firstName").filter($"firstName" === "John").first().get(0)

    assert(flattenDF === "John")
  }
  test("test that dataframe is created") {}
  test("test that filter result is correct") {}
}
