package aba.fileParser

import scala.util.{Success, Failure, Try, Using}
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.{Map => MutableMap}
import aba.framework.{Contrary, Framework, Literal, Rule}

// These classes are just for the purpose of parsing.
case class PConstraint(lit: Literal)
case class PAssumption(lit: Literal)
case class PGoal(lit: Literal)



abstract class FileParser extends RegexParsers{

  val alphabet: MutableMap[String, Literal]  =  MutableMap.empty[String, Literal]

  def comment: Parser[String] = """%(.*)""".r ^^ { _.toString }
  def identifier: Parser[String] = """[a-zA-Z_$][a-zA-Z_$0-9]*""".r ^^ { _.toString }
  def fields: Parser[Set[Literal]] = (literal~",".?).* ^^ { _.map(_._1).toSet } // create a sorted set out of a set
  def literal: Parser[Literal] = identifier ^^ { id =>
    if (alphabet.contains(id)) alphabet(id)
    val newLiteral = Literal(id)
    alphabet += (id -> newLiteral)
    newLiteral
  }

  def rule: Parser[Rule]
  def contrary: Parser[Contrary]
  def assumption: Parser[PAssumption]
  def goal: Parser[PGoal]
  def constraint: Parser[PConstraint]


  def parseFile(fileName: String): Try[Framework] = {
    Using(Source.fromFile(fileName, enc="UTF-8")) {
      source => {

        val parsedObjects = source.getLines.map(parseAll(rule | contrary | assumption | constraint | goal | comment, _) match {
          case Success(e, _) => e
          case _ => ()
        }).toSet

        val assumptions = parsedObjects.collect{ case a: PAssumption => a.lit }
        val rules = parsedObjects.collect{ case r: Rule => r }
        val contraries = parsedObjects.collect{ case c: Contrary => c }
        val goals = parsedObjects.collect{ case g: PGoal => g.lit }
        val constraints = parsedObjects.collect{ case c: PConstraint => c.lit }

        // TODO: from here I started optimizing
        val actualAssumptions = assumptions.map(_.id)
        val actualGoals = goals.map(_.id)
        val actualConstraints = constraints.map(_.id)

        // self contradicting assumptions




        new Framework(rules, actualAssumptions, contraries, actualGoals, actualConstraints, alphabet.keys.toSet)

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
