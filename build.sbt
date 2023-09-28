import scala.sys.process.*
import scala.language.postfixOps

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

lazy val root = (project in file("."))
  .settings(
    scalacOptions ++= List("-source:future"),
    name         := "kinetic-merge",
    organization := "com.sageserpent",
    packageExecutable := {
      val _ = publishLocal.value; (rabinFingerprint / publishLocal).value

      val localArtifactCoordinates =
        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"

      val executablePath = s"${target.value}${Path.sep}${name.value}"

      s"cs bootstrap --verbose --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !

      name.value
    },
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.6",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core"  % "3.2.0",
      "dev.optics" %% "monocle-macro" % "3.2.0"
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "com.lihaoyi"     %% "fansi"     % "0.4.0",
    libraryDependencies += "com.sageserpent" %% "americium" % "1.16.0" % Test,
    libraryDependencies += "com.lihaoyi"     %% "pprint"    % "0.8.1"  % Test,
    libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test,
    libraryDependencies += "net.aichler" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
    Test / fork               := true,
    Test / testForkedParallel := true,
    Test / javaOptions ++= Seq("-Xms10G", "-Xmx10G")
  )
  .dependsOn(rabinFingerprint)

lazy val rabinFingerprint = (project in file("rabinfingerprint")).settings(
  packageDoc / publishArtifact             := false,
  libraryDependencies += "com.google.guava" % "guava" % "32.1.2-jre",
  libraryDependencies += "junit"            % "junit" % "4.13.2" % Test
)
