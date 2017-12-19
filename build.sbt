import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.skyseb",
      scalaVersion := "2.12.3",
      version      := "0.1.0"
    )),
    name := "fpis",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  )
