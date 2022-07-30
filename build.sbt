ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

publish / skip := true

lazy val scandas = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "scandas",
    version := "0.1.0-pre.32",
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
      "io.github.edadma" %%% "importer" % "0.1.11",
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
