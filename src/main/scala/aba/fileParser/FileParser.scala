package aba.fileParser

import scala.util.{Success, Failure, Try, Using}
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.{Map => MutableMap}
import aba.framework.{Contrary, Framework, Literal, Rule}

// These classes are just for the purpose of parsing.
case class PConstraint(lit: String)
case class PAssumption(lit: String)
case class PGoal(lit: String)



abstract class FileParser extends RegexParsers{

  def identifier: Parser[String] = """[a-zA-Z_$][a-zA-Z_$0-9]*""".r ^^ { _.toString }
  def fields: Parser[Set[String]] = (identifier~",".?).* ^^ { _.map(_._1).toSet } // create a sorted set out of a set

  def rule: Parser[Rule]
  def contrary: Parser[Contrary]
  def assumption: Parser[String]
  def goal: Parser[String]
  def constraint: Parser[String]


  def parseLines[A](parser: Parser[A])(implicit lines: List[String]): Set[A] = {
    lines.map(parseAll[A](parser, _)).filter {
      case Success(_, _) => true
      case _ => false
    }.map(_.get).toSet
  }


  def parseFile(fileName: String): Try[Framework] = {
    Using(Source.fromFile(fileName, enc="UTF-8")) {
      source => {

        implicit val sourceLines: List[String] = source.getLines.toList

        new Framework(
          rules = parseLines(rule),
          assumptions = parseLines(assumption),
          contraries = parseLines(contrary),
          goals = parseLines(goal),
          constraints = parseLines(constraint)
        )
      }
    }
  }
}

object FileParser {
  def apply(parserType: String, filePath: String, goalOpt: Option[String] = None): Framework = {
    val parser: FileParser = parserType match {
      case "apx" => ApxParser
      case "aba" => AbaParser
    }

    parser.parseFile(filePath) match {
      case Success(fram) => goalOpt match {
        case Some(goal) => fram.copy(goals = Set(goal))
        case _ => fram
      }
      case Failure(exception) => throw exception
    }
  }
}
