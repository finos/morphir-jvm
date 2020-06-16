import $ivy.`org.scalameta::scalameta:4.3.14`
import $ivy.`com.lihaoyi::pprint:0.5.9`

import scala.meta._
val program = """object Main extends App { print("Hello!") }"""
val tree    = program.parse[Source].get

pprint.pprintln(tree)

val caseClass = """
    case class Person(firstName:String, lastName:scala.String, dob:java.time.LocalDate, lastVaccine: scala.concurrent.duration.Duration)
"""

val ccTree = caseClass.parse[Source].get
pprint.pprintln(ccTree)
