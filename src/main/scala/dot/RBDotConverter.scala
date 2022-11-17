package dot

import aba.framework.{Contrary, Framework, Rule}
import aba.reasoner.DisputeState
import dot.ABDotConverter.getLabel

import java.io.PrintWriter

object RBDotConverter {

  private def getUid(string: String, appendInfo: Boolean = true): String = {
    val hCode = string.hashCode
    val uid = if (hCode > 0) "a" + hCode.toString else "an" + Math.abs(hCode).toString
    if (appendInfo) uid + s"_$string" else uid
  }

  private def getUid(rule: Rule): String = {
    getUid(rule.head, appendInfo = false) + getUid(rule.body.hashCode().toString, appendInfo = false) + s"_${rule.head}_rule"
  }

  private def getColor(colorString: String, secondGradientColor: String = "white")(implicit isGradientFill: Boolean) = s"${if (isGradientFill) s"$secondGradientColor:" else ""}${colorString}"

  private val nodeDefaults = s"""node [ fontname="times-bold", color="black", style="filled", fontsize="20" margin="0.1,0.0"  ]  """


  // attributes
  private val parentAttr = s""" label="" """
  private val factAttr = s""" label="" """

  // colors
  private val proponentColor = "white:green"
  private val defencesColor = "white:green"
  //private val defencesColor = "white:green4"
  private val opponentColor = "white:yellow"
  private val culpritColor = "white:red"
  private val blockedColor = "white:grey"
  private val unAttackedOpponentsAssumptionsColor = "white:yellow" // was deepping
  private val blockedRemainingRulesColor = "white:grey"
  //private val goalColor = "white:deepskyblue"
  private val goalColor = "white:green"
  private val parentColor = "white:yellow"

  // shapes
  private val parentShape = """ shape="circle", width=0.15 """
  private val ruleShape = """ shape="hexagon" """
  private val factShape = """ shape="star", width=0.5"""
  private val assumptionShape = """shape="diamond", width=0.75, height=0.75"""
  private val statementShape = """shape="rectangle" """
  private val goalShape = """ shape="invtriangle" """

  // arrows styles
  private val attackArrowStyle = """ fillcolor="white", arrowhead="onormal" """
  private val propAttackArrowColor = """ color="red:white:red" """ // was black
  private val oppAttackArrowColor = """ color="red:white:red"  """


  def getRuleLabel(rule: Rule): String = {

    val maxLength = 7
    val arrow = "←"
    val initialElems = if (rule.head.length > maxLength) List(rule.head + s" $arrow ") else List(rule.head, s"$arrow ")

   rule.body.foldLeft(initialElems) { (acc, next) =>
      if (acc.last.length > maxLength) {
          acc :+ next
      } else {
        val lastElem = acc.last
        val newElem = if (lastElem.trim ==arrow) lastElem + next else lastElem + ", " + next
        acc.dropRight(1) :+ newElem
      }
    }.mkString("\n")

    //"a"
  }

  def exportDotRepr(gradientFill: Boolean = true, outputFileName: String = "temp_rule.dot", showBlocked: Boolean = false)(implicit dState: DisputeState, framework: Framework): Unit = {
    val reprString = getDotString(gradientFill, outputFileName, showBlocked)

    new PrintWriter(outputFileName) { write(reprString); close() }
  }

  def exportLegend(gradientFill: Boolean = true): String = {
    val legString = getLegendString(gradientFill)
    val legFileName = "legend.dot"

    new PrintWriter(legFileName) { write(legString); close() }
    legFileName
  }

  private def getLegendString(gradientFill: Boolean = true): String = {

    implicit val isGradientFill: Boolean = gradientFill

    s"""digraph Legend {
       |${"\t"} node [ fontname="times-bold", color="black", style="filled", fontsize="20" margin="0.2,0", group="a" ]
       |${"\t"} edge[style=invis]
       |
       |${"\t"}// NODES
       |${"\t"} defence [ label="Defence", fillcolor="${getColor(defencesColor)}", $assumptionShape ]
       |${"\t"} propRule [ label="Prop. rule", fillcolor="${getColor(proponentColor)}", $ruleShape ]
       |${"\t"} propStatement [ label="Prop. statement", fillcolor="${getColor(proponentColor)}", $statementShape ]
       |${"\t"} goal [ label="Goal", fillcolor="${getColor(goalColor)}", $statementShape ]
       |
       |${"\t"} culprit [ label="Culprit", fillcolor="${getColor(culpritColor)}", $assumptionShape ]
       |${"\t"} oppRule [ label="Opp. rule", fillcolor="${getColor(opponentColor)}", $ruleShape ]
       |${"\t"} oppStatement [ label="Opp. statement", fillcolor="${getColor(opponentColor)}", $statementShape ]
       |${"\t"} oppAssumption [ label="Non-attacked opp. assumption", fillcolor="${getColor(unAttackedOpponentsAssumptionsColor)}", $assumptionShape ]
       |${"\t"} blockRemRule [ label="Blocked remaining rule", fillcolor="${getColor(blockedRemainingRulesColor)}", $ruleShape ]
       |${"\t"} playedBlockPiece [ label="Played blocked piece", fillcolor="${getColor(blockedColor)}", $statementShape ]
       |
       |${"\t"} defence -> propRule -> propStatement -> goal
       |${"\t"} culprit -> oppRule -> oppStatement -> oppAssumption -> playedBlockPiece -> blockRemRule
       |
       |${"\t"}	d1 -> d2 -> d3 -> d4 -> d5 -> d6
       |${"\t"}	edge [ arrowhead="normal", arrowtail="dot", style="filled"];
       |
       |${"\t"} d1 [ label="", shape="circle", style="filled", fillcolor="white" ]
       |${"\t"} d2 [ label="", shape="circle", style="filled", fillcolor="white" ]
       |${"\t"} d1 -> d2 [ label="   child", fontsize="20", fontname="times-bold"]
       |
       |${"\t"} d3 [ label="", shape="circle", style="filled", fillcolor="white" ]
       |${"\t"} d4 [ label="", shape="circle", style="filled", fillcolor="white" ]
       |${"\t"} edge [ arrowhead="normal", arrowtail="dot", style="filled", $attackArrowStyle, $propAttackArrowColor]
       |${"\t"}	d3 -> d4 [ label="   proponent attack", fontsize="20", fontname="times-bold"]
       |
       |${"\t"} d5 [ label="", shape="circle", style="filled", fillcolor="white" ]
       |${"\t"} d6 [ label="", shape="circle", style="filled", fillcolor="white" ]
       |${"\t"}	edge [ arrowhead="normal", arrowtail="dot", style="filled", $attackArrowStyle, $oppAttackArrowColor]
       |${"\t"} d5 -> d6 [ label="   opponent attack", fontsize="20", fontname="times-bold"]
       |
       |}""".stripMargin
  }

  private def getDotString(gradientFill: Boolean = true, outputFileName: String = "output_rule.dot", showBlocked: Boolean = false)
                          (implicit dState: DisputeState, framework: Framework): String = {

    implicit val isGradientFill: Boolean = gradientFill

    val allBlockedRules = framework.rules.filter{
      case Rule(_, _, body) => (body intersect dState.culprits).nonEmpty
    }

    val statements = if (showBlocked) dState.bStatements ++ allBlockedRules.flatMap(_.statements) else dState.bStatements


    val statementsNodes = statements.map { st =>
      s"${getUid(st)} [ " +
        s"""label="${getLabel(st)}", """  +
        s"""fillcolor="${ st match {
          case s if framework.goals.contains(s) => goalColor
          case s if dState.defences.contains(s) => defencesColor
          case s if dState.culprits.contains(s) => culpritColor // should not happen if we don't show blocked
          case s if framework.assumptions.contains(s) => unAttackedOpponentsAssumptionsColor
          case s if dState.pStatements.contains(s) => proponentColor
          case s if dState.bPlayedBlockedStatements(s) => blockedColor // should not happen if we don't show blocked
          case s if !dState.bStatements.contains(s) => blockedRemainingRulesColor // should not happen if we don't show blocked
          case _ => opponentColor
        }
        }",""" +
          s" ${st match {
            case s if framework.assumptions.contains(s) => assumptionShape
            case s if framework.goals.contains(s) => goalShape
            case _ => statementShape
          }}  ] "
    }

    val consideredRules = if (showBlocked) dState.bRules ++ allBlockedRules else dState.bRules

    val rulesNodes = consideredRules.map(rule =>
      s"${getUid(rule)} [ " +
        s"""label="${getRuleLabel(rule)}", """  +
        s"""fillcolor="${
          rule match {
            case r if dState.pRules.contains(r) => proponentColor
            case r if dState.bPlayedBlockedRules.contains(r) => blockedColor
            case r if !dState.bRules.contains(r) => blockedRemainingRulesColor
            case _ => opponentColor
          }}",""" +
        s"${ruleShape} ]"
    )

    val ruleParentsNodes = consideredRules.map(rule =>
      s"""${getUid(rule)}p [ $parentAttr, $parentShape, fillcolor="$parentColor" ]"""
    )

    val ruleBodyToRuleParentEdges = consideredRules.map{
      case rule@Rule(_, _, body) =>
        s""" { ${body.map(getUid(_)).mkString(", ")} } -> ${getUid(rule)}p"""
    }

    // additional, for facts
    val factsNodes = consideredRules.filter{
      case Rule(_, _, body) => body.isEmpty
    }.map(rule => s""" ${getUid(rule)}f [ $factAttr, $factShape, fillcolor="${if (dState.pRules.contains(rule)) proponentColor else opponentColor }"  ]""")

    val factsToRuleParentsEdges = consideredRules.filter{
      case Rule(_, _, body) => body.isEmpty
    }.map(rule => s"""${getUid(rule)}f -> ${getUid(rule)}p""")

    val ruleParentToRuleEdges = consideredRules.map(rule =>
      s"""${getUid(rule)}p -> ${getUid(rule)}"""
    )

    // only those that have rules for them
    val statementsParentsNodes = consideredRules.map {
      case Rule(_, head, _) => s"""${getUid(head)}p [ $parentAttr, $parentShape, fillcolor="$parentColor" ]"""
    }

    val statementsParentToStatementsEdges = consideredRules.map {
      case Rule(_, head, _) => s"""${getUid(head)}p -> ${getUid(head)}"""
    }

    val ruleHeadsToStatementsParentsEdges = consideredRules.map {
      case r@Rule(_, head, _) => s"""${getUid(r)} -> ${getUid(head)}p"""
    }

    val additionalCulprits = if (showBlocked) (dState.culprits -- dState.bStatements) else Set.empty[String]

    val nonUsedCulpritsNodes = additionalCulprits.map { st =>
      s"${getUid(st)} [ " +
        s"""label="${getLabel(st)}", """  +
        s"""fillcolor="$culpritColor" """ +
        s"$assumptionShape ] "
    }

    val culpritAttacked = if (showBlocked) dState.culprits else dState.culprits intersect dState.bStatements

    val proponentsAttackEdges = culpritAttacked.map( c =>
      s"""{ ${(framework.contraries.filter { case Contrary(asm, _) => c == asm }.map(_.contrary) intersect dState.pStatements).map(getUid(_)).mkString(", ")} } -> ${getUid(c)} [ $attackArrowStyle, $propAttackArrowColor ] """
    )

    val opponentAttacks = if (showBlocked) {
      (dState.defenceContraries intersect (dState.bStatements union allBlockedRules.flatMap(_.statements)))
    } else {
      dState.defenceContraries intersect dState.bStatements
    }

    val opponentAttackEdges = opponentAttacks.map(ctr =>
      s"""${getUid(ctr)}  -> { ${(framework.contraries.filter { case Contrary(_, c) => c == ctr }.map(_.assumption) intersect dState.defences).map(getUid(_)).mkString(", ")} } [ $attackArrowStyle, $propAttackArrowColor ] """
    )


    s"""digraph DisputeStateRuleRepresentation {
       |${"\t"}$nodeDefaults
       |
       |${"\t"}rankdir="BT";
       |
       |
       |${"\t"}// NODES
       |${"\t"}// statements nodes
       |${"\t" + statementsNodes.mkString("\n\t")}
       |${"\t"}// rules nodes
       |${"\t" + rulesNodes.mkString("\n\t")}
       |${"\t"}// rules parents nodes
       |${"\t" + ruleParentsNodes.mkString("\n\t")}
       |${"\t"}// statements parents nodes
       |${"\t" + statementsParentsNodes.mkString("\n\t")}
       |${"\t"}// facts nodes
       |${"\t" + factsNodes.mkString("\n\t")}
       |${"\t"}// not uttered culprits nodes
       |${"\t" + nonUsedCulpritsNodes.mkString("\n\t")}
       |
       |${"\t"}// EDGES
       |${"\t"}// Rule Parent -> Rule
       |${"\t" + ruleParentToRuleEdges.mkString("\n\t")}
       |${"\t"}// rule body -> rule parent
       |${"\t" + ruleBodyToRuleParentEdges.mkString("\n\t")}
       |${"\t"}// statement parent -> statement
       |${"\t" + statementsParentToStatementsEdges.mkString("\n\t")}
       |${"\t"}// rule head -> statement parent
       |${"\t" + ruleHeadsToStatementsParentsEdges.mkString("\n\t")}
       |${"\t"}// fact -> rule parent
       |${"\t" + factsToRuleParentsEdges.mkString("\n\t")}
       |${"\t"}// proponent attack edges
       |${"\t" + proponentsAttackEdges.mkString("\n\t")}
       |${"\t"}// opponent attack edges
       |${"\t" + opponentAttackEdges.mkString("\n\t")}
       |
       |}""".stripMargin
  }
}
