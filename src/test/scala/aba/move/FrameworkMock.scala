package aba.move

import aba.framework.Framework

object FrameworkMock {


  def bicycleFrameworkPath: (String, String) = ("apx", "examples/bicycle.apx")
//  def frameworkExtractor: ((String, String)) => Framework = (Framework _).tupled

//  def bicycleFramework: Framework = frameworkExtractor(bicycleFrameworkPath)

  def bicycleFramework: Framework = Framework.apply(bicycleFrameworkPath._1, bicycleFrameworkPath._2)
}
