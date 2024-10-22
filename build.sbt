import scala.language.postfixOps

lazy val javaVersion = "17"

ThisBuild / scalaVersion := "3.3.4"

ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)

ThisBuild / scalacOptions += s"-java-output-version:$javaVersion"

lazy val root = (project in file("."))
  .settings(
    scalacOptions ++= List("-source:future"),
    name                                   := "kinetic-merge",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
  )
