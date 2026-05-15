ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"
libraryDependencies += "org.scala-lang.modules"%% "scala-parallel-collections"%"1.2.0"
lazy val root = (project in file("."))
  .settings(
    name := "ProjetoPPM"
  )
