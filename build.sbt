
import sbt.Keys.{libraryDependencies, mainClass}
import sbt._

// name := "ScaCASL2-ScaCOMET2"
lazy val _version = "0.1"
// scalaVersion := "2.12.3"


lazy val pushName = settingKey[String]("test name")
lazy val pushKey = settingKey[String]("test key")

lazy val push = taskKey[Unit]("push zip to S3 for CodeDeploy Test")
// resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

lazy val commonSettings = Seq (
  version := _version,
  scalaVersion := "2.12.3",
  libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "com.github.pathikrit" %% "better-files" % "3.1.0"
    )
  /*,
  push := {
    TaskTest.push(pushName.value, pushKey.value)
  } */
)

lazy val root = (project in file(".")).
  aggregate(scacasl2).
  aggregate(scacomet2).
  settings(
    name := "ScaCASL2-ScaCOMET2",
    pushName := "scala CASL2/COMET2",
    pushKey := "scala CASL2/COMET2",
  )


lazy val scacasl2 = (project in file("sca-casl2")).
  settings(commonSettings: _*).
  settings (
    name := "sca-casl2",
    pushName := "sca-cals2",
    pushKey := "sca-cals2",
    mainClass in assembly := Some("scacasl2.ScaCasl2"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
    )
  )


lazy val scacomet2 = (project in file("sca-comet2")).
  dependsOn(scacasl2).
  settings(commonSettings: _*).
  settings(
    name := "sca-comet2",
    pushName := "sca-comet2",
    pushKey := "sca-comet2",
    mainClass in assembly := Some("scacomet2.ScaComet2"),
  )
