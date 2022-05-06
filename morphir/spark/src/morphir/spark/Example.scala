package morphir.spark



import org.apache.spark.sql.SparkSession


case class Department(id: String, name: String)
case class Employee(firstName: String, lastName: String, email: String, salary: Int)
case class DepartmentWithEmployees(department: Department, employees: Seq[Employee])


object Example extends App {

 // val sqlContext = new org.apache.spark.sql.SQLContext(sc)
  val spark = SparkSession.builder().appName("Example").getOrCreate()
  import spark.implicits._


  val department = Department("1234", "Computer Science")
  val employee = Employee("Nunya", "Klah", "nunya@mail.com", 9000)
  val employee2 = Employee("Ebuka", "Choong", "ebuka@mail.com", 2000)

  val departmentWithEmployees = DepartmentWithEmployees(department, Seq(employee, employee2))

  //creating dataframe

  val departmentWithEmployeesSeq = Seq(departmentWithEmployees)

  val df1 = departmentWithEmployeesSeq.toDF()
  df1.show()

 //flatten employee class into columns

 val flattenDF = explodeDF.select($"col.*")
 flattenDF.show()

 //filter rows

 val filterDF = flattenDF.filter($"firstName" === "Ebuka").sort($"lastName".asc)
 display(filterDF)


}
