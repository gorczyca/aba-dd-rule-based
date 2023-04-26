name := "flexable"

version := "1.0"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test
libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.1.0"
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.17.2"

assembly / mainClass := Some("Main")
