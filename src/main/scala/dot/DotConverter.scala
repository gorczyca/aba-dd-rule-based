package dot

import aba.framework.{Framework, Rule}
import aba.reasoner.DisputeState

import java.io.PrintWriter

object DotConverter {

  //private def getAbsHashcode(hashCode: Int): String = {
  //  if (hashCode > 0) "a" + hashCode.toString else "an" + Math.abs(hashCode).toString
  //}

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
  private val proponentColor = "green"
  private val defencesColor = "green4"
  private val opponentColor = "yellow"
  private val culpritColor = "red"
  private val blockedColor = "orange"
  private val unattackedOpponentsAssumptionsColor = "deeppink"
  private val blockedRemainingRulesColor = "grey"
  private val goalColor = "deepskyblue"
  private val parentColor = "yellow"

  // shapes
  private val parentShape = """ shape="circle", width=0.15 """
  private val ruleShape = """ shape="hexagon" """
  private val factShape = """ shape="star", width=0.5"""
  private val assumptionShape = """shape="diamond", width=0.75, height=0.75"""
  private val statementShape = """shape="rectangle" """

  // arrows styles
  private val attackArrowStyle = """ fillcolor="white", arrowhead="onormal" """
  private val propAttackArrowColor = """ color="black:white:black" """
  private val oppAttackArrowColor = """ color="red:white:red"  """


  def exportDotRepr(gradientFill: Boolean = true, outputFileName: String = "temp_rule.dot")(implicit dState: DisputeState, framework: Framework): Unit = {
    val reprString = getDotString(gradientFill)

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
       |${"\t"} oppAssumption [ label="Non-attacked opp. assumption", fillcolor="${getColor(unattackedOpponentsAssumptionsColor)}", $assumptionShape ]
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

  private def getDotString(gradientFill: Boolean = true, outputFileName: String = "output_rule.dot")(implicit dState: DisputeState, framework: Framework): String = {


    implicit val isGradientFill: Boolean = gradientFill

    val nodeDefaults = s"""node [ fontname="times-bold", color="black", style="filled", fontsize="20" margin="0.1,0.0"  ]  """

    val defences = dState.defences
    //val attackedByP = framework.contraries.filter(ctr => dState.pStatements.contains(ctr.contrary)).map(_.assumption) // TODO: this is just culprits ?

    val unattackedOpponentsAssumptions = ((dState.oStatements intersect framework.assumptions) -- dState.defences) -- dState.culprits


    //val oStatements = dState.oStatements -- dState.pStatements

    //val remainingBlockedRulesWithHeadsInOpponentsStatements = dState.bPlayedBlockedRules.filter {
    //  case Rule(_, head, _) => oStatements.contains(head)
    //} // TODO: to chyba kazdy??

    // nodes for those rules
    // TODO: here these UID
    /* PREVIOUSLY */
    //val remainingBlockedRulesNodes = remainingBlockedRulesWithHeadsInOpponentsStatements.map(
    //  rule => s"""${rule.uid} [  label="$rule", fillcolor="${getColor(blockedRemainingRulesColor)}" $ruleShape]"""
    //)
    /* NOW */
    val remainingBlockedRulesNodes = dState.bPlayedBlockedRules.map(
      rule => s"""${getUid(rule)} [  label="$rule", fillcolor="${getColor(blockedRemainingRulesColor)}" $ruleShape]"""
    )

    // TODO: naming should be better
    // parent edges from blocked rule arguments to parents of statements in opponent set
    /* PREVIOUSLY */
    //val parentEdges4 = remainingBlockedRulesWithHeadsInOpponentsStatements.groupBy(_.head)
    //  .map { case (head, rules) => s"""{ ${rules.map(_.uid).mkString(", ")} } -> ${head}p""" }

    /* NOW */
    val parentEdges4 = dState.bPlayedBlockedRules.groupBy(_.head)
      .map { case (head, rules) => s"""{ ${rules.map(getUid).mkString(", ")} } -> ${getUid(head)}p""" }

    // additional edges from parents of statements (rules) to statements,
    // for those statements that have no "actual" parents (??? what is an actual parent here?)
    // so no parents except for the remaining blocked rules
    val statementsWithNoParentsExceptForBlockedRules = dState.bPlayedBlockedRules.groupBy(_.head)
      .map { case (head, _) => head }
      .filter(stmt => parents(stmt).isEmpty)


    val parentNodes2 = statementsWithNoParentsExceptForBlockedRules.map(stmt => s"""${getUid(stmt)}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]""")
    val parentEdges5 = statementsWithNoParentsExceptForBlockedRules.map(stmt => s"""${getUid(stmt)}p -> ${getUid(stmt)}""")

    // pairs - arguments (that have children) and those children
//    val parentNodesTuplesOld = dState.b.filter {
//      case arg => arg.parents.nonEmpty  // arguments that have parents
//      //case ruleArg: RuleArgument => ruleArg.rule.body.isEmpty // facts
//      case _ => false   // TODO: is it necessary?
//    }.map(arg =>
//      (arg, s"""${arg.uid}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]""")
//    )



    val parentNodesTuples = (dState.bStatements ++ dState.bRules).filter {
      case stmt: String => parents(stmt).nonEmpty
      case rule: Rule => parents(rule).nonEmpty // TODO: better, can i just simply call parents on any?
    }.map {
      case stmt: String => (stmt, getUid(stmt))
      case rule: Rule => (rule, getUid(rule))
    }.map {
      case (stmtOrRule, id) => (stmtOrRule, s"""${id}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]""")
    }

    // TODO: here i had parents including culprits, but do i really need it?
    val parentEdges3 = dState.bPlayedBlockedRules.filter(rule => parentsWithCulprits(rule).nonEmpty)
      .map(rule => s"""{ ${ parentsWithCulprits(rule).map(getUid(_)).mkString(", ") } } -> ${getUid(rule)}p  """)

    val remainingBlockedRulesParentNodesTuples = dState.bPlayedBlockedRules
      .filter(rule => parentsWithCulprits(rule).nonEmpty)
      .map(rule => (rule, s""" ${getUid(rule)}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ] """) )

    val parentNodes = (parentNodesTuples ++ remainingBlockedRulesParentNodesTuples).map(_._2)

    val parentEdges1 = (parentNodesTuples ++ remainingBlockedRulesParentNodesTuples).map {
      case (stmt: String, _) => getUid(stmt)
      case (rule: Rule, _) => getUid(rule)
    }.map ( id => s"""${id}p -> $id """ )

    // facts
    val facts = dState.bRules.filter(_.body.isEmpty)

    // stars
    val factNodes = facts.map (rule =>
      s""" ${getUid(rule)}f [ $factAttr, $factShape, fillcolor="${if (dState.pRules.contains(rule)) getColor(proponentColor) else getColor(opponentColor) }"  ]"""
    )

    // skip the rule argument, when there is an empty body. i.e.
    // for a rule s :- we should obtain
    // ☆ -> ○ -> s
    // instead of
    // ☆ -> ○ -> <s:-> -> ○ -> s
    val parentFactNodes = facts.map ( rule => s"""${getUid(rule.head)}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]"""  )
    // parent edges for facts
    val factEdges = facts.map(rule => s"${getUid(rule)}f -> ${getUid(rule)}p")

    val parentEdges2 = (dState.bRules ++ dState.bStatements).filter {
      case stmt: String =>  parents(stmt).exists {
        case Rule(_, _, body) => body.nonEmpty
      }
      case rule: Rule => parents(rule).nonEmpty // TODO: might be a problem here
    }.map {
      case stmt: String => s""" { ${parents(stmt).filter {
        case Rule(_, _, body) => body.nonEmpty
      }.map(getUid).mkString(" ") } } -> ${getUid(stmt)}p """
      case rule: Rule => s""" { ${parents(getUid(rule)).mkString(", ") } } -> ${getUid(rule)}p"""
    }

    // parent edges from B + culprits to blocked rules
                                                                                  // no need to filter for non empty bodies of parents, since it has to have a body containing a culprit
    // for performance reasons
    //lazy val playedBlockedPieces = framework.playedBlockedPieces

    val bStatementsNodes = dState.bStatements.map( st =>
      s"${getUid(st)} [ " +
        s"""label="${st}", """  +
        s"""fillcolor="${
          getColor(st match {
            case s if framework.goals.contains(s) => goalColor
            case s if dState.defences.contains(s) => defencesColor
            case s if dState.pStatements.contains(s) => proponentColor
            case s if unattackedOpponentsAssumptions.contains(s) => unattackedOpponentsAssumptionsColor
            case s if dState.bPlayedBlockedStatements.contains(s) => blockedColor
            case _ => opponentColor
        })}",""" +
        s" ${st match {
          case s if framework.assumptions.contains(s) => assumptionShape
          case _ => statementShape
        }}  ] "
    )

    val bRulesNodes = dState.bRules.filter {
      case Rule(_, _, body) => body.nonEmpty  // do not draw rule arguments with empty bodies
    }.map( rule =>
      s"${getUid(rule)} [ " +
        s"""label="${rule.toString}", """  +
        s"""fillcolor="${getColor( rule match {
          case r if dState.pRules.contains(r) => proponentColor
          case r if dState.bPlayedBlockedRules.contains(r) => blockedColor
          case _ => opponentColor
        })
        }",""" +
        s" ${ruleShape}  ] "
    )


//    val argumentNodes = dState.b.filter {
//      case ruleArg: RuleArgument => ruleArg.rule.body.nonEmpty  // do not draw rule arguments with empty bodies
//      case _: LiteralArgument => true
//    }.map( arg =>
//      s"${arg.uid} [ " +
//        s"""label="${arg.toString}", """  +
//        s"""fillcolor="${getColor( arg match {
//          case litArg: LiteralArgument if framework.goals.contains(litArg.lit) => goalColor
//          case litArg: LiteralArgument if defencesArgs.contains(litArg) => defencesColor
//          case a if dState.p.contains(a) => proponentColor
//          case litArg: LiteralArgument if unattackedOpponentsAssumptions.contains(litArg) => unattackedOpponentsAssumptionsColor
//          case a if playedBlockedPieces.contains(a) => blockedColor
//          case _ => opponentColor
//        })
//        }",""" +
//        s" ${arg match {
//          case _: RuleArgument => ruleShape
//          case litArgument: LiteralArgument if framework.assumptions.contains(litArgument.lit) => assumptionShape
//          case _ => statementShape
//        }}  ] "
//    )

    val culpritNodes = dState.culprits.map(st =>
      s""" ${getUid(st)} [ label="${st}", $assumptionShape, fillcolor="${getColor(culpritColor)}" ] """
    )

    val propAttacks = (dState.culprits ++ framework.contrariesOf(dState.defences))
      .map(st =>
        (st, dState.pStatements.intersect(framework.contrariesOf(st))))
      .filter(_._2.nonEmpty)
      .map{
        case (st, attackingSt) =>
          s""" { ${attackingSt.map(getUid(_)).mkString(" ")} } -> ${getUid(st)} [ $attackArrowStyle, $propAttackArrowColor ] """
      }

    val defenceContraries = dState.defenceContraries // framework.contrariesOf(framework.defences)
    val playedDefenceContraries = dState.bStatements intersect defenceContraries //.filter(arg => defenceContraries.contains(arg.lit))

    val oppAttacks = dState.defences //dState.pLitArgs.filter(litArg => framework.defences.contains(litArg.lit))
      .map(d =>
        (d, playedDefenceContraries.intersect(framework.contrariesOf(d))))
      .filter(_._2.nonEmpty)
      .map{
        case (d, dAttackers) =>
          s""" { ${dAttackers.map(getUid(_)).mkString(", ")} } -> ${ getUid(d) } [ $attackArrowStyle, $oppAttackArrowColor ] """
      }

    s"""digraph DisputeStateRuleRepresentation {
       |${"\t"}$nodeDefaults
       |
       |${"\t"}// NODES
       |${"\t"}// parent nodes
       |${"\t" + parentNodes.mkString("\n\t")}
       |${"\t" + parentNodes2.mkString("\n\t")}
       |
       |${"\t"}// fact nodes
       |${"\t" + factNodes.mkString("\n\t")}
       |${"\t" + parentFactNodes.mkString("\n\t")}
       |${"\t" + remainingBlockedRulesNodes.mkString("\n\t")}
       |${"\t"}// rules nodes
       |${"\t" + bStatementsNodes.mkString("\n\t")}
       |${"\t"}// rules nodes
       |${"\t" + bRulesNodes.mkString("\n\t")}
       |${"\t"}// culprit nodes
       |${"\t" + culpritNodes.mkString("\n\t")}
       |
       |${"\t"}// EDGES
       |${"\t"}// parent edges
       |${"\t" + parentEdges1.mkString("\n\t")}
       |${"\t" + parentEdges2.mkString("\n\t")}
       |${"\t" + parentEdges3.mkString("\n\t")}
       |${"\t" + parentEdges4.mkString("\n\t")}
       |${"\t" + parentEdges5.mkString("\n\t")}
       |
       |${"\t"}// fact edges
       |${"\t" + factEdges.mkString("\n\t")}

       |
       |${"\t"}// proponent attack edges
       |${"\t" + propAttacks.mkString("\n\t")}
       |
       |${"\t"}// opponent attack edges
       |${"\t" + oppAttacks.mkString("\n\t")}
       |
       |}""".stripMargin
  }


  private def parents(statement: String)(implicit dState: DisputeState): Set[Rule] = {
    dState.bRules.filter {
      case Rule(_, head, _) => head == statement
    }
  }

  private def parents(rule: Rule)(implicit dState: DisputeState): Set[String] = {
    dState.bStatements intersect rule.body
  }


  private def parentsWithCulprits(rule: Rule)(implicit dState: DisputeState): Set[String] = {
    (dState.bStatements ++ dState.culprits) intersect rule.body
  }



  private def children(statement: String) = {

  }

  private def children(rule: Rule) = {

  }


}
