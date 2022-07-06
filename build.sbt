//name := "aba-dd-rule"
name := "flexABle"

//version := "0.1"
version := "1.0"

scalaVersion := "2.13.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.0.0"
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.17.1"

mainClass in assembly := Some("Main")
