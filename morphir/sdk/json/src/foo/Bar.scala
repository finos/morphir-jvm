package foo

sealed trait Bar {

}

case class Baz(arg1 : String) extends Bar
case class Bat(arg1 : String, arg2 : Int) extends Bar


// type Bar = Baz String | Bat String Int

// [ "Baz", "arg1" ] or [ "Bat", "arg1", 2 ]