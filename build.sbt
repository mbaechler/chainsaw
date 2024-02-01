ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "mb"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "rosetree",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.21",
      "dev.zio" %% "zio-test" % "2.0.21" % Test,
      "org.typelevel" %% "cats-core" % "2.10.0" % Test,
      "dev.zio" %% "zio-interop-cats" % "23.1.0.0"
    )
  )
