val scala212 = "2.12.10"
val scala213 = "2.13.2"
val supportedScalaVersions = List(scala212, scala213)

inThisBuild(
  List(
    scalaVersion := scala213,
    scalafmtOnCompile := true,
    organization := "com.github.lukajcb",
    homepage := Some(url("https://github.com/lukajcb/cats-bi-io")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "LukaJCB",
        "Luka Jacobowitz",
        "luka.jacobowitz@gmail.com",
        url("https://github.com/LukaJCB")
      )
    )
  )
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Use("olafurpg", "setup-gpg", "v2"),
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)
ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(List("fmtCheck", "test")))

ThisBuild / crossScalaVersions := supportedScalaVersions
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-Xlint:infer-any",
  "-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Xfatal-warnings"
)

addCommandAlias("fmtAll", ";scalafmt; test:scalafmt; scalafmtSbt")
addCommandAlias("fmtCheck", ";scalafmtCheck; test:scalafmtCheck; scalafmtSbtCheck")

lazy val commonSettings = Seq(
  addCompilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

val catsEffectV = "2.1.4"
val disciplineMunitV = "0.2.3"

lazy val root = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "cats-bi-io",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectV,
      "org.typelevel" %% "cats-effect-laws" % catsEffectV % Test,
      "org.typelevel" %% "discipline-munit" % disciplineMunitV % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
