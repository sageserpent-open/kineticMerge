import scala.sys.process.*
import scala.language.postfixOps

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val packageExecutable = taskKey[String]("Package an executable with Coursier")


lazy val root = (project in file("."))
  .settings(
    name := "kinetic-merge",
    organization := "com.sageserpent",
    packageExecutable := {
      val _ = publishLocal.value

      val moduleId = organization.value %% name.value % version.value

      val localArtifactCoordinates = s"${moduleId.organization}::${moduleId.name}:${moduleId.revision}"

      s"cs bootstrap -f $localArtifactCoordinates -o ${target.value}${Path.sep}${name.value}" !

      name.value
    }
  )
