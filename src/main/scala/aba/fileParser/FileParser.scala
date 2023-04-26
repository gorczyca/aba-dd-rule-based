package aba.fileParser

import scala.util.{Try, Using}
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import aba.framework.{Contrary, Framework, Rule}

/** Base parser class. */
abstract class FileParser extends RegexParsers {

  /** Returns string parser of an identifier: a letter (or an _ or $) followed by
   *
   * @return String parser of an identifier.
   */
  def identifier: Parser[String] = """[a-zA-Z_$0-9]+""".r
  // def identifier: Parser[String] = """[a-zA-Z_$][a-zA-Z_$0-9]*""".r ^^ { _.toString }
  // TODO: also allow for atoms, function symbols etc
  // def identifier: Parser[String] = """\w+(\(\w+(,\w+)*\))?""".r ^^ { _.toString }

  /** Returns a parser of a set of identifiers.
   *
   * @return Parser of a set of identifiers.
   */
  def fields: Parser[Set[String]] = (identifier~",".?).* ^^ { _.map(_._1).toSet } // create a sorted set out of a set

  /** Returns a parser of a [[Rule]].
   *
   * @return Rule parser.
   */
  def rule: Parser[Rule]

  /** Returns a parser of a [[Contrary]].
   *
   * @return Contrary parser.
   */
  def contrary: Parser[Contrary]

  /** Returns a string parser of an assumption.
   *
   * @return Assumption string parser.
   */
  def assumption: Parser[String]

  /** Returns a string parser of a goal.
   *
   * @return Goal string parser.
   */
  def goal: Parser[String]

  /** Returns a string parser of a constraint.
   *
   * @return Constraint string parser.
   */
  def constraint: Parser[String]

  /** Parses lines of the input file and returns a set of framework elements of type A.
   *
   * @param parser A [[Parser]]
   * @param lines Lines of the input file.
   * @tparam A Returned type of elements of the set. Can be a string (if parsing goals or assumptions), [[Rule]] or a [[Contrary]].
   * @return
   */
  private def parseLines[A](parser: Parser[A])(implicit lines: List[String]): Set[A] = {
    lines.map(parseAll[A](parser, _)).filter {
      case Success(_, _) => true
      case _ => false
    }.map(_.get).toSet
  }


  /** Parses the input file and returns the new framework.
   *
   * @param fileLines lines of the input file.
   * @return A resulting [[Framework]]
   */
  private def parseFileLines(implicit fileLines: List[String]): Framework = {
    new Framework(
      rules = parseLines(rule),
      assumptions = parseLines(assumption),
      contraries = parseLines(contrary),
      goals = parseLines(goal),
      constraints = parseLines(constraint)
    )
  }
}

/** FileParser companion object. */
object FileParser {
  /**
   *
   * @param parserType Parser type, currently "apx" or "aba".
   * @param filePath Path to the input file.
   * @return New framework if no errors occurred while reading the file.
   */
  def apply(parserType: String, filePath: String): Try[Framework] = {
    val parser: FileParser = parserType match {
      case "apx" => ApxParser
      case "aba" => AbaParser
      case "iccma" => IccmaParser
    }

    Using(Source.fromFile(filePath, enc = "UTF-8")) {
      source => {

        val sourceLines = source.getLines().toList
        parser.parseFileLines(sourceLines)

      }
    }
  }
}
