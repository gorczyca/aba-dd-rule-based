package aba.reasoner.guided

import config.AppConf
import pureconfig.ConfigSource

import sys.process._
import pureconfig.generic.auto._
import pureconfig._

import scala.collection.mutable.ListBuffer
import scala.sys.process

object GuidedReasoner {

  def main(args: Array[String]): Unit = {

    val c = ConfigSource.default.load[AppConf]

    val properties = c match {
      case Right(x) => x
    }

    val command1 = s"cmd /C ${properties.clingoPath} ${properties.instancePath} 0"
//    val re2 = command1.!
//
//    val stdout = new StringBuilder
//    val stderr = new StringBuilder
    //val status = command1 ! ProcessLogger(line => ())


    val list = new ListBuffer[String]()

    val s= command1 ! ProcessLogger{
      line => list += line
    }

    val firstLine = list.indexWhere(line => line.toLowerCase.startsWith("solving"))
    val lastLine = list.indexWhere(line => line.toLowerCase.contains("satisfiable"))

    val answerSetsUnfiltered = list.slice(firstLine+1, lastLine)
    val answerSetsFiltered = answerSetsUnfiltered.filterNot(line => line.toLowerCase.startsWith("answer"))
    val answerSets = answerSetsFiltered.map { line =>
      line.split(" ").collect {
        //TODO: change motive to "in"
        case s"motive($x)" => x
      }
    }



//
//    println(status)
//    println("stdout: " + stdout)
//    println("stderr: " + stderr)


    //val res1 = "cmd.exe echo".!!

//    val result = command.!
//
 ///   val x = 1

    val command = Seq("dir")


    //val command = Seq("protractor", "--version")
    val os = sys.props("os.name").toLowerCase
    val panderToWindows = os match {
      case x if x contains "windows" => Seq("cmd", "/C") ++ command
      case _ => command
    }

    val res = panderToWindows.mkString(" ").!

    //val contents = Process("cmd /C dir").lazyLines.mkString


    val x = 1




  }


}
