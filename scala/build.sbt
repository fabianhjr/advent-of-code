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

lazy val globalDependencies = Seq()

lazy val advent =
  crossProject(JVMPlatform, NativePlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .settings(sharedSettings)
    .settings(
      name := "advent",
      libraryDependencies ++= globalDependencies
    )
