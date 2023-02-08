name := "flexable"

version := "1.0"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.0.0"
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.17.1"

assembly / mainClass := Some("Main")
