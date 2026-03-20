import sbt._
import sbt.Keys.{libraryDependencies, mainClass}

// name := "ScaCASL2-ScaCOMET2"
lazy val _version = "0.1"
// scalaVersion := "2.12.3"


lazy val commonSettings = Seq (
  version := _version,
  scalaVersion := "3.3.0",
  libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.15",
  "org.scalatest" %% "scalatest" % "3.2.15" % "test",
     // Removed better-files dependency
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
    )
)

lazy val root = (project in file(".")).
  aggregate(scacasl2).
  aggregate(scacomet2).
  settings(
    name := "ScaCASL2-ScaCOMET2",
  )


lazy val scacasl2 = (project in file("sca-casl2")).
  settings(commonSettings: _*).
  settings (
    name := "sca-casl2",
    assembly / mainClass := Some("scacasl2.ScaCasl2"),
    // ...existing code...
  )


lazy val scacomet2 = (project in file("sca-comet2")).
  dependsOn(scacasl2).
  settings(commonSettings: _*).
  settings(
    name := "sca-comet2",
    assembly / mainClass := Some("scacomet2.ScaComet2"),
  )
