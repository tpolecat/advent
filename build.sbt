
lazy val kindProjectorVersion     = "0.8.0"
lazy val scalazVersion            = "7.2.7"
lazy val matryoshkaVersion        = "0.14.0"

lazy val commonSettings = Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-Ypartial-unification"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion)
)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalaz"   %% "scalaz-core"     % scalazVersion,
      "org.scalaz"   %% "scalaz-effect"     % scalazVersion,
      "com.slamdata" %% "matryoshka-core" % matryoshkaVersion,
      "org.tpolecat" %% "atto-core"  % "0.5.1",
      "org.tpolecat" %% "atto-compat-scalaz72" % "0.5.1"
    ),
    initialCommands := """
      import scalaz._, Scalaz._
      import matryoshka._
      import matryoshka.implicits._
      import matryoshka.data._
      import matryoshka.patterns._
    """.trim
  )

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .dependsOn(core)
  .aggregate(core)
