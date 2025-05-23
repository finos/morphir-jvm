package morphir.codegen.tasty

import com.typesafe.scalalogging.{Logger, StrictLogging}
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.FormatVersion

import java.nio.file.{Files, Paths}
import scala.quoted.*
import scala.tasty.inspector.*
import scala.util.{Failure, Success}

class TastyToMorphir(morphirPath: String) extends Inspector with StrictLogging {
  def inspect(using quotes: Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {

    given Contexts.Context = quotes.asInstanceOf[runtime.impl.QuotesImpl].ctx

    for (tasty <- tastys) {
      val tree = tasty.ast
      tree match {
        case tr: dotty.tools.dotc.ast.Trees.Tree[?] =>
          tr.toVersionedDistribution match {
            case Success(distribution) =>
              writeDistribution(distribution)
            case Failure(ex) =>
              logger.error(ex.getMessage, ex)
          }
      }
    }
  }

  private def writeDistribution(distribution: FormatVersion.VersionedDistribution): Unit = {
    val encodedPackageDef = morphir.ir.formatversion.Codec.encodeVersionedDistribution(distribution)
    val jsonBytes = encodedPackageDef.noSpaces.getBytes("UTF-8")
    Files.write(Paths.get(morphirPath), jsonBytes)
    logger.info(s"IR written to $morphirPath")
  }
}

@main def tastyToMorphirIR(morphirIROutputPath: String, tastyFiles: String*): Unit = {
  val logger = Logger("morphir.codegen.tasty.TastyToMorphir")
  logger.info(s"Provided IR Output Path: $morphirIROutputPath")
  logger.info(s"Provided TASTy files: $tastyFiles")

  if (tastyFiles.size > 1) {
    throw new UnsupportedOperationException(s"Currently only 1 TASTy file can be parsed, but ${tastyFiles.size} was provided")
  }

  val tastyFilesList = List(tastyFiles *)
  val tastyInspector = new TastyToMorphir(morphirIROutputPath)
  TastyInspector.inspectTastyFiles(tastyFilesList)(tastyInspector)
}