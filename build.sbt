name := "sentential"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.tpolecat" %% "atto-core"  % "0.5.2",
  "org.tpolecat" %% "atto-compat-cats" % "0.5.2",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)
