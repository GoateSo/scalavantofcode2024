val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "scalavantofcode2024",
    version      := "0.1.0-SNAPSHOT",
    fork         := true,
    scalaVersion := scala3Version,
    run / javaOptions += "-Xmx8G",
    scalacOptions += "-deprecation",
    libraryDependencies += "com.lihaoyi" %% "os-lib"    % "0.9.0",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.2",
    libraryDependencies += "com.lihaoyi" %% "pprint"    % "0.8.1",
    libraryDependencies += "com.lihaoyi" %% "fansi"     % "0.4.0"
  )