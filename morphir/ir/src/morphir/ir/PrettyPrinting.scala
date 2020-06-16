package morphir.ir

import pprint.PPrinter

object PrettyPrinting extends PrettyPrinting

trait PrettyPrinting {
  val pretty: PPrinter = pprint.copy(
    additionalHandlers = {
      case value: name.Name => pprint.Tree.Literal(value.show)
    }
  )
}
