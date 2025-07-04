ThisBuild / licenses += "ISC"      -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.7.1"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.1"
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/tabula"),
    "scm:git@github.com:edadma/tabula.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage := Some(url("https://github.com/edadma/tabula"))

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value // ← This is the key difference!
}

lazy val tabula = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "tabula",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    organization                            := "io.github.vinctustech",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
//    libraryDependencies ++= Seq(
//      "com.github.scopt" %%% "scopt" % "4.1.0",
//    ),
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "csv"      % "0.0.1",
      "io.github.edadma" %%% "importer" % "0.0.8",
      "io.github.edadma" %%% "matrix"   % "0.0.2",
      "io.github.edadma" %%% "table"    % "0.0.1",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
    licenses += "ISC"      -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js"  %% "scalajs-stubs" % "1.1.0" % "provided",
    libraryDependencies += "com.lihaoyi"  %%% "pprint"        % "0.9.0" % "test",
    libraryDependencies += "org.postgresql" % "postgresql"    % "42.7.7",
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js"       %% "scalajs-stubs"   % "1.1.0" % "provided",
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer      := false,
    Test / scalaJSUseTestModuleInitializer      := true,
    scalaJSUseMainModuleInitializer             := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )

lazy val root = project
  .in(file("."))
  .aggregate(tabula.js, tabula.jvm, tabula.native)
  .settings(
    name                := "tabula",
    publish / skip      := true,
    publishLocal / skip := true,
  )
