ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "main"
  )

libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.19") // .withDottyCompat(scalaVersion.value)
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.8"
