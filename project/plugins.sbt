addSbtPlugin("com.github.sbt.junit" % "sbt-jupiter-interface" % "0.19.0")
addSbtPlugin("io.stryker-mutator"   % "sbt-stryker4s"         % "0.21.0")
// NOTE: should reinstate this, but let's see if this has a bearing on the latest CI failure...
//addSbtPlugin("org.scalameta"        % "sbt-scalafmt"          % "2.6.1")
addSbtPlugin("ch.epfl.scala"  % "sbt-version-policy" % "3.3.0")
addSbtPlugin("com.github.sbt" % "sbt-release"        % "1.5.0")
addSbtPlugin("org.scoverage"  % "sbt-scoverage"      % "2.4.4")
