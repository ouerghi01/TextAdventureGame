val scala3Version = "3.5.0"

lazy val root = (project in file("."))
  .settings(
    name := "Scala_Quest",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    // Enable debugging
    javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005",

    // Add test dependencies
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
