ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "fr.baechler-craftsmanship"
ThisBuild / organizationName := "chainsaw"


ThisBuild / githubWorkflowJavaVersions += JavaSpec.temurin("17")

inThisBuild(List(
  homepage := Some(url("https://github.com/mbaechler/chainsaw")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "mbaechler",
      "Matthieu Baechler",
      "matthieu@baechler-craftsmanship.fr",
      url("https://github.com/mbaechler")
    )
  )
))

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish project"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"

lazy val root = (project in file("."))
  .settings(
    name := "chainsaw",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.21",
      "dev.zio" %% "zio-test" % "2.0.21" % Test,
      "org.typelevel" %% "cats-core" % "2.10.0" % Test,
      "dev.zio" %% "zio-interop-cats" % "23.1.0.0"
    )
  )
