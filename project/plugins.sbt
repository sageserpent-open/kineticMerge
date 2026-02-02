addSbtPlugin("com.github.sbt.junit" % "sbt-jupiter-interface" % "0.17.0")
addSbtPlugin("io.stryker-mutator"   % "sbt-stryker4s"         % "0.19.1")
// NOTE: pin the dependency on `sbt-scalafmt` to 2.5.5 due to this project's build
// invoking Coursier directly - as that plugin also depends on Coursier, this forces
// resolution to choose a recent version of Coursier that is only built for Scala 2.13
// and Scala 3; that won't work with SBT 1.* which runs on Scala 2.12.
addSbtPlugin("org.scalameta"  % "sbt-scalafmt"       % "2.5.5")
addSbtPlugin("ch.epfl.scala"  % "sbt-version-policy" % "3.2.1")
addSbtPlugin("com.github.sbt" % "sbt-release"        % "1.4.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype"       % "3.12.2")
addSbtPlugin("org.scoverage"  % "sbt-scoverage"      % "2.4.4")
