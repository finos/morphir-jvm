package org.morphir.workspace.config

import java.io.File

import com.typesafe.config.ConfigFactory
import izumi.reflect.Tag
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe._
import zio.config.{ ConfigSource, _ }
import zio.{ Has, Task, ZIO, ZLayer }

/**
 * Property resolution order:
 * - command line arguments
 * - system properties
 * - environment variables
 * - HOCON file
 * - properties file
 */
object ConfigLayer {

  def createLayer[T: Tag](
    args: List[String],
    descriptor: ConfigDescriptor[T]
  ): ZIO[Any, Throwable, ZLayer[Any, ReadError[String], Has[T]]] =
    for {
      sources <- createSources(args)
      desc    = descriptor.from(unifySources(sources))
      l       = ZLayer.fromEffect(ZIO.fromEither(read(desc)))
    } yield l

  private def unifySources(sources: List[ConfigSource]): ConfigSource =
    sources.reduce((s1, s2) => s1.orElse(s2))

  private def createSources(args: List[String]): ZIO[Any, Throwable, List[ConfigSource]] = {
    val NO_PROFILE = ""
    val PROD       = "prod"
    for {
      argsConfigSource  <- ZIO.succeed(ConfigSource.fromCommandLineArgs(args, Some('.'), Some(',')))
      systemPropsSource <- ConfigSource.fromSystemProperties(Some('_'), Some(','))
      envPropsSource    <- ConfigSource.fromSystemEnv(Some('_'), Some(','))
      profile           = getProfile(unifySources(List(argsConfigSource, systemPropsSource, envPropsSource)))
      appHoconSource <- profile.hoconFile match {
                         case Some(value) =>
                           fromHoconResource(s"/$value")
                         case None =>
                           fromHoconResourceIfPresent(
                             profile.profile.map(_.toLowerCase()).getOrElse(NO_PROFILE) match {
                               case NO_PROFILE => "/morphir-workspace.conf"
                               case PROD       => "/morphir-workspace.conf"
                               case profile    => s"/morphir-workspace-$profile.conf"
                             }
                           )
                       }
      appPropsSource <- profile.propertiesFile match {
                         case Some(value) =>
                           fromPropertiesResource(s"/$value", Some('.'), Some(','))
                         case None =>
                           fromPropertiesResourceIfPresent(
                             profile.profile.map(_.toLowerCase()).getOrElse(NO_PROFILE) match {
                               case NO_PROFILE => "/morphir-workspace.properties"
                               case PROD       => "/morphir-workspace.properties"
                               case profile    => s"/morphir-workspace-$profile.properties"
                             },
                             Some('.'),
                             Some(',')
                           )
                       }
    } yield List(argsConfigSource, systemPropsSource, envPropsSource, appHoconSource, appPropsSource)
  }

  /**
   * Will fail if the file is not found.
   *
   * @param file
   * @param keyDelimiter
   * @param valueDelimiter
   * @return
   */
  private def fromPropertiesResource[A](
    file: String,
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): Task[ConfigSource] =
    for {
      properties <- ZIO.bracket(
                     ZIO.effect(getClass.getResourceAsStream(file))
                   )(r => ZIO.effectTotal(r.close())) { inputStream =>
                     ZIO.effect {
                       val properties = new java.util.Properties()
                       properties.load(inputStream)
                       properties
                     }
                   }
    } yield ConfigSource.fromProperties(
      properties,
      file,
      keyDelimiter,
      valueDelimiter
    )

  /**
   * Will not fail if file is not found. Instead it will create a ConfigSource from an empty java.util.Properties
   *
   * @param file
   * @param keyDelimiter
   * @param valueDelimiter
   * @return
   */
  private def fromPropertiesResourceIfPresent[A](
    file: String,
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): Task[ConfigSource] =
    for {
      //scalafix:off
      properties <- ZIO.bracket(
                     ZIO.effect(getClass.getResourceAsStream(file))
                   )(r => ZIO.effectTotal(if (r != null) r.close())) { inputStream =>
                     ZIO.effect {
                       val properties = new java.util.Properties()
                       if (inputStream != null) {
                         properties.load(inputStream)
                       }
                       properties
                     }
                   }
      //scalafix:on
    } yield ConfigSource.fromProperties(
      properties,
      file,
      keyDelimiter,
      valueDelimiter
    )

  /**
   * Will fail if the file is not found.
   *
   * @param file
   * @return
   */
  private def fromHoconResource[A](file: String): Task[ConfigSource] =
    for {
      resourceURI <- ZIO
                      .fromOption(Option(getClass.getResource(file)).map(_.toURI))
                      .mapError(_ => new RuntimeException(s"$file not found in classpath!"))
      fileInstance <- Task(new File(resourceURI))
      configSource <- ZIO
                       .fromEither(
                         TypesafeConfigSource.fromTypesafeConfig(ConfigFactory.parseFile(fileInstance).resolve)
                       )
                       .mapError(error => new RuntimeException(error))
    } yield configSource

  /**
   * Will not fail if file is not found. Instead it will create a ConfigSource from an empty HOCON string
   *
   * @param file
   * @return
   */
  private def fromHoconResourceIfPresent[A](file: String): Task[ConfigSource] =
    Option(getClass.getResource(file)).map(_.toURI) match {
      case Some(uri) =>
        Task(new File(uri)).flatMap(fileInstance =>
          TypesafeConfigSource
            .fromTypesafeConfig(ConfigFactory.parseFile(fileInstance).resolve) match {
            case Left(value)  => Task.fail(new RuntimeException(value))
            case Right(value) => Task.succeed(value)
          }
        )
      case None => Task.succeed(ConfigSource.empty)
    }

  private final case class Profile(
    profile: Option[String],
    hoconFile: Option[String],
    propertiesFile: Option[String]
  )

  private def getProfile(configSource: ConfigSource): Profile = {
    val desc   = descriptor[Profile]
    val params = desc.from(configSource)
    read(params) match {
      case Left(_)      => Profile(None, None, None)
      case Right(value) => value
    }
  }

}
