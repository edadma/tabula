ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / scalaVersion := "3.7.1"
ThisBuild / organization := "io.github.edadma"
ThisBuild / organizationName := "edadma"
ThisBuild / organizationHomepage := Some(url("https://github.com/edadma"))
ThisBuild / version := "0.0.1"
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
    version := "0.0.2",
    scalaVersion := "3.1.3",
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
    organization := "io.github.vinctustech",
    githubOwner := "vinctustech",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
//    libraryDependencies ++= Seq(
//      "com.github.scopt" %%% "scopt" % "4.1.0",
//    ),
    resolvers += Resolver.githubPackages("edadma"),
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "csv" % "0.1.4",
      "io.github.edadma" %%% "importer" % "0.0.8",
      "io.github.edadma" %%% "json" % "0.1.13",
      "io.github.edadma" %%% "matrix" % "0.1.2",
      "io.github.edadma" %%% "table" % "1.0.3",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.7.3" % "test",
    libraryDependencies += "org.postgresql" % "postgresql" % "42.4.0",
  )
  .nativeSettings(
    nativeLinkStubs := true,
    libraryDependencies += "io.github.cquiroz" % "scala-java-time_native0.4_3" % "2.4.0",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.4.0",
  )

lazy val root = project
  .in(file("."))
  .aggregate(tabula.js, tabula.jvm, tabula.native)
  .settings(
    name := "tabula",
    publish / skip := true,
    publishLocal / skip := true,
  )
