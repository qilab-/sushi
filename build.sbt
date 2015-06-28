name := """sushi"""

organization := "jp.qilab"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

scalacOptions in Compile ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-Ywarn-dead-code", "-Ywarn-numeric-widen")
