package aba.move

import aba.fileParser.FileParser
import aba.framework.Framework

import scala.util.{Failure, Success}

object FrameworkMock {


  def bicycleFrameworkPath: (String, String) = ("apx", "examples/bicycle.apx")
//  def frameworkExtractor: ((String, String)) => Framework = (Framework _).tupled

//  def bicycleFramework: Framework = frameworkExtractor(bicycleFrameworkPath)

  def bicycleFramework: Framework = FileParser(bicycleFrameworkPath._1, bicycleFrameworkPath._2) match {
    case Success(value) => value
    case Failure(exception) => throw exception
  }
}
