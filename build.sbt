import scala.sys.process.*
import scala.language.postfixOps

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

lazy val root = (project in file("."))
  .settings(
    name         := "kinetic-merge",
    organization := "com.sageserpent",
    packageExecutable := {
      val _ = publishLocal.value

      val localArtifactCoordinates =
        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"

      val executablePath = s"${target.value}${Path.sep}${name.value}"

      s"cs bootstrap --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !

      name.value
    }
  )
