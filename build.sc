import mill._, scalalib._
import Dependencies._

val crossVersions = Seq("2.13.8", "3.1.2")

object morphir extends Module {
  object knowledge                                     extends mill.Cross[KnowledgeModule](crossVersions: _*) {}
  class KnowledgeModule(val crossScalaVersion: String) extends MorphirCrossScalaModule                        {
    // def ivyDeps = Agg(com.softwaremill.common.tagging)
    object test extends Tests with MorphirZioTestModule {}
  }
}

trait MorphirCrossScalaModule extends CommonCrossModule {}

trait MorphirZioTestModule extends CommonTestModule {}

trait CommonCrossModule extends CrossScalaModule {
  def scalacOptions = T.task {
    val extraOptions = if (this.crossScalaVersion.startsWith("2.")) {
      Seq("-Yrangepos")
    } else {
      Seq()
    }
    super.scalacOptions() ++ extraOptions
  }
}

trait CommonTestModule extends TestModule {
  def ivyDeps       = super.ivyDeps() ++ Agg(dev.zio.zio, dev.zio.`zio-test`, dev.zio.`zio-test-sbt`)
  def testFramework = "zio.test.sbt.ZTestFramework"
}

object Dependencies {
  case object com {
    case object softwaremill {
      case object common {
        val tagging = ivy"com.softwaremill.common::tagging::2.3.3"
      }
    }
  }
  case object dev {
    case object zio {
      val version             = "1.0.14"
      val zio: Dep            = ivy"dev.zio::zio::${version}"
      val `zio-prelude`       = ivy"dev.zio::zio-prelude::${version}"
      val `zio-test`          = ivy"dev.zio::zio-test::${version}"
      val `zio-test-magnolia` = ivy"dev.zio::zio-test-magnolia::${version}"
      val `zio-test-sbt`      = ivy"dev.zio::zio-test-sbt::${version}"
    }
  }
  case object org {
    case object scalameta {
      private val munitVersion     = "1.0.0-M4"
      val munit: mill.scalalib.Dep = ivy"org.scalameta::munit::$munitVersion"
      println(s"$munit")
      val `munit-scalacheck` =
        ivy"org.scalameta::munit-scalacheck::${munitVersion}"

    }
  }
}
