val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "pps-23-24-lab07",
    version                                := "0.1.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test,
    // add scala test
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
