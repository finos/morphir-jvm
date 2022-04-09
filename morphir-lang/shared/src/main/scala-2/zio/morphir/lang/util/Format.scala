package zio.morphir.lang.util

import scala.util.{ Try, Success, Failure }

object Format {
  def apply(code: String, showErrorTrace: Boolean = false) = {
      val encosedCode =
        s"""|object DummyEnclosure {
            |  ${code}
            |}""".stripMargin

      // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
      //       use only for debugging purposes!
      def unEnclose(enclosedCode: String) =
        val lines =
          enclosedCode
            .replaceFirst("^object DummyEnclosure \\{", "")
            .reverse
            .replaceFirst("\\}", "")
            .reverse
            .split("\n")
        val linesTrimmedFirst = if (lines.head == "") lines.drop(1) else lines
        // if there was a \n} on the last line, remove the }
        val linesTrimmedLast = if (linesTrimmedFirst.last == "") linesTrimmedFirst.dropRight(1) else linesTrimmedFirst
        // then if all lines had at least one indent i.e. "  " remove that
        if (linesTrimmedLast.forall(line => line.startsWith("  ")))
          linesTrimmedLast.map(line => line.replaceFirst("  ","")).mkString("\n")
        else
          linesTrimmedLast.mkString("\n")

      val formatted =
        Try {
          // val formatCls = classOf[ScalafmtFormat.type]
          // val result = formatCls.getMethod("apply").invoke(null, encosedCode)
          // println("============ GOT HERE ===========")
          // val resultStr = s"${result}"
          // resultStr
          Scalafmt(encosedCode)
        }.getOrElse {
          println("====== WARNING: Scalafmt Not Detected ====")
          encosedCode
        }

      unEnclose(formatted)
    }
}