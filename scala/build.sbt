val sharedSettings = Seq(
  scalaVersion := "3.2.1",
  scalacOptions ++= Seq(
    "-deprecation",
    "-explain",
    "-explain-types",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings"
  )
)

wartremoverErrors ++= Warts.unsafe

lazy val advent =
  crossProject(JVMPlatform, NativePlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .settings(sharedSettings)
    .settings(
      name := "advent",
      libraryDependencies ++= Seq(
        "co.fs2"        %%% "fs2-core"    % "3.4.0",
        "co.fs2"        %%% "fs2-io"      % "3.4.0",
        "org.typelevel" %%% "cats-core"   % "2.9.0",
        "org.typelevel" %%% "cats-effect" % "3.4.2",
        "org.typelevel" %%% "cats-parse"  % "0.3.8"
      )
    )
    .jsSettings(
      Compile / scalaJSUseMainModuleInitializer := true
    )
