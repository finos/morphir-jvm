package morphir.cli

import java.nio.file.{Path => JPath}
import zio._
import zio.console._
import zio.process._
import zio.blocking.`package`.Blocking
import zio.nio.core.file.Path
import zio.stream.ZStream
import morphir.sdk.{ModelLoader, modelLoader}

object Cli {
  trait Service {
    def generateScala(
        modelPath: JPath,
        output: JPath
    ): ZIO[ModelLoader with Blocking with Console, Throwable, Unit]

    def elmMake(
        projectDir: Option[JPath] = None,
        output: Option[JPath] = None,
        help: Boolean = false
    ): ZStream[Blocking, Throwable, String]
  }

  val live = ZLayer.succeed {
    new Service {

      def generateScala(modelPath: JPath, output: JPath): ZIO[
        morphir.sdk.ModelLoader with Blocking with Console,
        Throwable,
        Unit
      ] =
        (for {
          json <- modelLoader.loadJsonFromFile(modelPath)
          _ <- putStrLn("JSON:")
          _ <- putStrLn(json.render(2))
        } yield ())

      def elmMake(
          projectDir: Option[JPath] = None,
          output: Option[JPath] = None,
          help: Boolean = false
      ) =
        (for {
          argsRef <- ZStream.fromEffect(Ref.make[Array[String]](Array.empty))
          _ <- ZStream.fromEffect(ZIO.whenCase(projectDir) {
            case Some(dir) =>
              argsRef.update(args =>
                args ++ Array("--project-dir", dir.toFile().getAbsolutePath())
              )
          })
          _ <- ZStream.fromEffect(ZIO.whenCase(output) {
            case Some(filePath) =>
              argsRef.update(args =>
                args ++ Array("--output", filePath.toFile().getAbsolutePath())
              )
          })
          args <- ZStream.fromEffect(argsRef.get)
          result <- Command("morphir-elm", args.toIndexedSeq: _*).linesStream
        } yield result)

    }
  }
}
