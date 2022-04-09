package zio.morphir.lang.util

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.{Formatted, Scalafmt => Fmt}
import ThrowableOps._

object Scalafmt {
  def apply(code: String, showErrorTrace: Boolean = false): String = {
    val style = ScalafmtConfig.default

    Fmt.format(code, style, Set.empty, "<input>") match {
      case Formatted.Success(formattedCode) =>
        formattedCode
      case Formatted.Failure(e) =>
        if (showErrorTrace)
          println(
            s"""===== Failed to format the code ====
               |$code
               |---
               |${e.stackTraceToString}.
               |""".stripMargin
          )
        code
    }
  }
}
