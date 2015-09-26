name := "scrabble"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings", // Someday!
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
  //"-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core" % "7.1.3"
 ,"org.scalacheck" %% "scalacheck"  % "1.12.5" % "test"
)

