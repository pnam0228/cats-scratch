ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.15"

lazy val root = (project in file("."))
  .settings(
    name := "scratch",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.scalatest" %% "scalatest" % "3.1.2" % Test
    ),
    scalacOptions += "-Ypartial-unification"
  )
