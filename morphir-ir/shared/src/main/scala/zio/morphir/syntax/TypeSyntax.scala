package zio.morphir.syntax
import zio.morphir.ir.Name
import zio.morphir.ir.TypeModule.Type.*
import zio.ZEnvironment

trait TypeSyntax {
  def defineVariable(name: String): Variable[Any] = Variable(Name.fromString(name), ZEnvironment.empty)
  def defineVariable(name: Name): Variable[Any]   = Variable(name, ZEnvironment.empty)
}

trait TypeModuleSyntax {
  def variable(name: String): Variable[Any] = Variable(Name.fromString(name), ZEnvironment.empty)
  def variable(name: Name): Variable[Any]   = Variable(name, ZEnvironment.empty)
}
