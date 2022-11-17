package commandLineParser

import aba.move.TerminationCriteria.{TA, TC, TS}
import aba.move.DisputeAdvancement.{DAB, DABF, DC, DS}
import aba.move.Move
import aba.move.Move.OF1
import aba.reasoner.automatic2.RuleChoice2
import aba.reasoner.automatic2.statementBased.{StatementChoice2, TurnChoice2}
import scopt.OParser


object CommandLineParser {

  // TODO: version etc
  private val builder = OParser.builder[ParserConfig]
  private val parser = {
    import builder._
    OParser.sequence(
      programName("[Program name]"),
      head("flexABle", "1.0"),
      //head("ABA-DD: Rule based.", "0.3"),
      // option -f, --file
      // TODO: File instead of string?
      arg[String]("<input file>")
        .required()
        //.valueName("<input file>")
        .action((x, c) => c.copy(inputFilePath = x)),
      //.text("path to input file"),
      opt[String]('i', "inputFormat")
        // TODO:
        .action((x, c) => c.copy(inputFormat = x))
        .validate {
          case "aba" | "apx" => success
          case _ => failure("Input format must be one of: aba or apx ")
        }
        .text("Input format. Possible values are aba (default) and apx")
        .valueName("<input format>"),
      opt[String]('g', "goal")
        .action((x, c) => c.copy(goal = Some(x))), // TODO: all options for strategies, termination criteria, advancement types
      // automatic reasoner options
      opt[String]("tc")
        .action((x, c) => {
          val tCriteria = x.toLowerCase match {
            case "ta" => TA
            case "tc" => TC
            case "ts" => TS
          }
          c.copy(tCriteriaType = tCriteria)
        }),
      opt[String]("da")
        .action((x, c) => {
          val dAdvancement = x.toLowerCase match {
            case "dab" => DAB
            case "dabf" => DABF
            case "dc" => DC
            case "ds" => DS
          }
          c.copy(dAdvancementType = dAdvancement)
        }),

      opt[String]("dfs")
        .action((x, c) => {
          val dfs = x.toLowerCase match {
            case "0" => false
            case "1" => true
          }
          c.copy(dfs = dfs)
        }),

      opt[String]("swa")
        .action((x, c) => {
          val swa = x.toLowerCase match {
            case "0" => false
            case "1" => true
          }
          c.copy(startWithAdmissible = swa)
        }),

      opt[String]("turn")
        .action((x, c) => {
          val turnChoice = x.toLowerCase match {
            case "p" => TurnChoice2.Proponent
            case "o" => TurnChoice2.Opponent
            case "s" => TurnChoice2.SmallestStatementSet
            case "l" => TurnChoice2.LargestStatementSet
          }
          c.copy(turnChoice = turnChoice)
        }), // TODO: should be removed?

      opt[String]("psc")
        .action((x, c) => {
          val psc = x.toLowerCase match {
            case "e" => StatementChoice2.Eager
            case "p" => StatementChoice2.Patient
            case "n" => StatementChoice2.Newest
            case "o" => StatementChoice2.Oldest
            case "lbce" => StatementChoice2.LBCEager
            case "lbcp" => StatementChoice2.LBCPatient
            case "gbce" => StatementChoice2.GBCEager
            case "gbcp" => StatementChoice2.GBCPatient
          }
          c.copy(pStatementChoice = psc) // TODO: some reusable mapping
        }),

      opt[String]("osc")
        .action((x, c) => {
          val osc = x.toLowerCase match {
            case "e" => StatementChoice2.Eager
            case "p" => StatementChoice2.Patient
            case "n" => StatementChoice2.Newest
            case "o" => StatementChoice2.Oldest
            case "lbce" => StatementChoice2.LBCEager
            case "lbcp" => StatementChoice2.LBCPatient
            case "gbce" => StatementChoice2.GBCEager
            case "gbcp" => StatementChoice2.GBCPatient
          }
          c.copy(oStatementChoice = osc)
        }),

      opt[String]("prc")
        .action((x, c) => {
          val prc = x.toLowerCase match {
            case "smin" => RuleChoice2.NewlyIntroducedStatementsMin
            case "smax" => RuleChoice2.NewlyIntroducedStatementsMax
            case "amin" => RuleChoice2.NewlyIntroducedAssumptionsMin
            case "amax" => RuleChoice2.NewlyIntroducedAssumptionsMax
            case "l1s" => RuleChoice2.LookAhead1Step
          }
          c.copy(pRuleChoiceType = prc)
        }),

      opt[String]("orc")
        .action((x, c) => {
          val orc = x.toLowerCase match {
            case "smin" => RuleChoice2.NewlyIntroducedStatementsMin
            case "smax" => RuleChoice2.NewlyIntroducedStatementsMax
            case "amin" => RuleChoice2.NewlyIntroducedAssumptionsMin
            case "amax" => RuleChoice2.NewlyIntroducedAssumptionsMax
            case "l1s" => RuleChoice2.LookAhead1Step
          }
          c.copy(oRuleChoiceType = orc)
        }),

    opt[String]("ordering")
      .action((x, c) => {
        val movesSeq = x.toLowerCase.grouped(3).map(Move.fromString).toSeq
        // make sure all are in there, OF1 should nevertheless be irrelevant
        val seq = if (movesSeq.contains(OF1)) movesSeq else OF1 +: movesSeq
        c.copy(preferenceOrdering = seq)
      }),

    //
    opt[String]("prc")
      .action((x, c) => {
        val prc = x.toLowerCase match {
          case "bmin" => RuleChoice2.BodyMin
          case "bmax" => RuleChoice2.BodyMax
          case "smin" => RuleChoice2.NewlyIntroducedStatementsMin
          case "smax" => RuleChoice2.NewlyIntroducedStatementsMax
          case "amin" => RuleChoice2.NewlyIntroducedAssumptionsMin
          case "amax" => RuleChoice2.NewlyIntroducedAssumptionsMax
          case "l1s" => RuleChoice2.LookAhead1Step
        }
        c.copy(pRuleChoiceType = prc)
      }),

    opt[String]("orc")
      .action((x, c) => {
        val orc = x.toLowerCase match {
          case "bmin" => RuleChoice2.BodyMin
          case "bmax" => RuleChoice2.BodyMax
          case "smin" => RuleChoice2.NewlyIntroducedStatementsMin
          case "smax" => RuleChoice2.NewlyIntroducedStatementsMax
          case "amin" => RuleChoice2.NewlyIntroducedAssumptionsMin
          case "amax" => RuleChoice2.NewlyIntroducedAssumptionsMax
          case "l1s" => RuleChoice2.LookAhead1Step
        }
        c.copy(oRuleChoiceType = orc)
      }))
  }

  def parse(args: Array[String]): Option[ParserConfig] = OParser.parse(parser, args, ParserConfig())

}
