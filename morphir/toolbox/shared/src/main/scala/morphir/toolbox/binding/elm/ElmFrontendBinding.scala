package morphir.toolbox.binding.elm

import morphir.toolbox.binding.FrontendBinding
import morphir.toolbox.core.{ Errors, Project }
import zio._
import zio.stream._

object ElmFrontendBinding {
  val BindingName = "elm"

  sealed trait ElmFrontendState
  object ElmFrontendState {
    final case class New(project: Project) extends ElmFrontendState
  }

  def make(project: Project): Task[ElmFrontendBinding] =
    ZIO.succeed(FrontendBinding(BindingName, Stream.empty, Stream.empty, ElmFrontendState.New(project)))

  private[elm] def createMorphirElmManifest(project: Project): ZIO[Any, Errors.NoSuchBinding, ElmManifest] =
    for {
      name     <- ZIO.succeed(project.name)
      binding  <- project.bindings.getAsEffect(BindingName)
      srcDir   = binding.srcDirs.head
      manifest = ElmManifest(name.toString, srcDir, List.empty)
    } yield manifest

}
