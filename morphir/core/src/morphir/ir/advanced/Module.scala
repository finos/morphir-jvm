package morphir.ir.advanced

object Module {
  case class Declaration[X](extra: X)
  case class Definition[X](extra: X)
}
