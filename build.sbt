name := "sentential"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.tpolecat" %%% "atto-core"  % "0.5.2",
  "org.tpolecat" %%% "atto-compat-cats" % "0.5.2",
  "com.lihaoyi" %%% "scalatags" % "0.6.5",
  "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
)

enablePlugins(ScalaJSPlugin)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalaJSUseMainModuleInitializer := true
