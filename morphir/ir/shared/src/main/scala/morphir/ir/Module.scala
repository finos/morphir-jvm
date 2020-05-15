package morphir.ir

object Module {
  final case class ModulePath(value: Path) extends AnyVal

  final case class Specification[+A](
    types: Map[Name, Type.Specification[A]],
    values: Map[Name, Value.Specification[A]]
  )

  object Specification {
    def empty[A]: Specification[A] = Specification[A](Map.empty, Map.empty)
  }

  final case class Definition[+A](
    types: Map[Name, AccessControlled[Type.Definition[A]]],
    values: Map[Name, AccessControlled[Value.Definition[A]]]
  ) {}

  object Definition {
    def empty[A]: Definition[A] = Definition[A](Map.empty, Map.empty)
  }
}
