package aba.fileParser

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class FileParserTest extends AnyFunSuite {

  val frameworkPathAba = "examples/unit_test_example.aba"
  val frameworkPathApx = "examples/unit_test_example.apx"

  test()

  def test(): Unit = {
    testAbaParser()
    testApxParser()
  }

  def testAbaParser(): Unit = {
    FileParser("aba", frameworkPathAba) match {
      case Success(framework) =>

        test("FileParser.Aba - assumptions") {
          assert(framework.assumptions == TestFrameworkMock.frameworkMock.assumptions)
        }

        test("FileParser.Aba - rules") {
          assert(framework.rules == TestFrameworkMock.frameworkMock.rules)
        }

        test("FileParser.Aba - contraries") {
          assert(framework.contraries == TestFrameworkMock.frameworkMock.contraries)
        }

        test("FileParser.Aba - goals") {
          assert(framework.goals == TestFrameworkMock.frameworkMock.goals)
        }

        test("FileParser.Aba - alphabet") {
          assert(framework.alphabet == TestFrameworkMock.frameworkMock.alphabet)
        }
      case Failure(exception) => throw exception
    }

  }
  def testApxParser(): Unit = {

    FileParser("apx", frameworkPathApx) match {
      case Success(framework) =>

        test("FileParser.Apx - assumptions") {
          assert(framework.assumptions == TestFrameworkMock.frameworkMock.assumptions)
        }

        test("FileParser.Apx - rules") {
          assert(framework.rules == TestFrameworkMock.frameworkMock.rules)
        }

        test("FileParser.Apx - contraries") {
          assert(framework.contraries == TestFrameworkMock.frameworkMock.contraries)
        }

        test("FileParser.Apx - goals") {
          assert(framework.goals == TestFrameworkMock.frameworkMock.goals)
        }

        test("FileParser.Apx - alphabet") {
          assert(framework.alphabet == TestFrameworkMock.frameworkMock.alphabet)
        }

      case Failure(exception) => throw exception
    }
  }

  def testMissingFile(): Unit = {

  }
}
