
name := "regex"

organization := "iguana"

version := "0.1.0"

scalaVersion := "2.11.7"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "junit" % "junit" % "4.11",
  "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val utils = ProjectRef(uri("https://github.com/iguana-parser/utils.git"), "utils")

val main = Project(id = "regex", base = file(".")).dependsOn(utils)

javacOptions in (Compile, doc) ++= Seq("-source", "1.8")
javacOptions in (Compile,doc) += "-Xdoclint:none"
