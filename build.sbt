name := "sqids-scala"
scalaVersion := "3.3.0"
libraryDependencies ++= Seq(
  "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
  "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test
)
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Wunused:all"
)
