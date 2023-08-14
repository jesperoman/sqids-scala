lazy val scala213 = "2.13.11"
lazy val scala3 = "3.3.0"
lazy val supportedScalaVersions = List(scala213, scala3)

ThisBuild / organization := "sqids"
ThisBuild / scalaVersion := scala213

lazy val sqids = (project in file("."))
  .settings(
    name := "sqids-scala",
    version := "0.1.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
    ),
    crossScalaVersions := supportedScalaVersions
  )
