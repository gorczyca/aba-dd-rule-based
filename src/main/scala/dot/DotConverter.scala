package dot

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, RuleArgument}

import java.io.PrintWriter

object DotConverter {

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


  def exportDotRepr(gradientFill: Boolean = true, outputFileNameOpt: Option[String] = None)(implicit dState: DisputeState, framework: Framework): String = {
    val reprString = getDotString(gradientFill)

    val outputFileName = outputFileNameOpt match {
      case Some(name) => name
      case _ => s"dispute_step${dState.id}.dot"
    }

    new PrintWriter(outputFileName) { write(reprString); close() }

    outputFileName
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

  private def getDotString(gradientFill: Boolean = true)(implicit dState: DisputeState, framework: Framework): String = {


    implicit val isGradientFill: Boolean = gradientFill

    val nodeDefaults = s"""node [ fontname="times-bold", color="black", style="filled", fontsize="20" margin="0.1,0.0"  ]  """

//    val parentAttr = s""" label="", shape="circle", fillcolor="${getColor("yellow")}", width=0.15 """
//    val ruleShape = """ shape="hexagon" """
//    val factShape = """label="", shape="star", width=0.5"""
//    val assumptionShape = """shape="diamond", width=0.75, height=0.75"""
//    val statementShape = """shape="rectangle" """
//    //val statementShape = """shape="square", width=0.6"""
//    val proponentColor = getColor("green")
//    val defencesColor = getColor("green4")
//    val opponentColor = getColor("yellow")
//    val culpritColor = getColor("red")
//    val blockedColor = getColor("orange")
//    val unattackedOpponentsAssumptionsColor = getColor("deeppink")
//    val blockedRemainingRulesColor = getColor("grey")
//    val goalColor = getColor("deepskyblue")

//    val attackArrowStyle = """ fillcolor="white", arrowhead="onormal" """
//    val propAttackArrowColor = """ color="black:white:black" """
//    val oppAttackArrowColor = """ color="red:white:red"  """

    val defencesArgs = dState.pLitArgs.filter(litArg => framework.defences.contains(litArg.lit))
    val unattackedOpponentsAssumptions = (dState.bLitArgs.filter(litArg => framework.assumptions.contains(litArg.lit)) -- defencesArgs)
      .filter(ass => !dState.bLitArgs.exists(litArg => framework.contrariesOf(ass.lit).contains(litArg.lit)))


    // remaining blocked rules with heads in the opponents set
    val opponentsSetLit = dState.bLitArgs -- dState.pLitArgs
    val remainingBlockedRulesWithHeadsInOpponentRuleArgs =
      framework.blockedRulesB.filter(rule => opponentsSetLit.map(_.lit).contains(rule.head))
        .map(RuleArgument)

    // nodes for those rules
    val remainingBlockedRulesNodes = remainingBlockedRulesWithHeadsInOpponentRuleArgs.map(
      ruleArg => s"""${ruleArg.uid} [  label="${ruleArg.toString}", fillcolor="${getColor(blockedRemainingRulesColor)}" $ruleShape]"""
    )

    // TODO: naming should be better
    // parent edges from blocked rule arguments to parents of statements in opponent set
    val parentEdges4 = remainingBlockedRulesWithHeadsInOpponentRuleArgs.groupBy(_.rule.head)
      .map { case (head, ruleArgs) => s"""{ ${ruleArgs.map(_.uid).mkString(", ")} } -> ${LiteralArgument(head).uid}p""" }

    // additional edges from parents of statements to statements, for those statements that have no "actual" parents
    // so no parents except for the remaining blocked rules
    val statementsWithNoParentsExceptForBlockedRules = remainingBlockedRulesWithHeadsInOpponentRuleArgs.groupBy(_.rule.head)
      .map { case (head, _) => LiteralArgument(head) }
      .filter(_.parents.isEmpty)

    val parentNodes2 = statementsWithNoParentsExceptForBlockedRules.map(litArg => s"""${litArg.uid}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]""")
    val parentEdges5 = statementsWithNoParentsExceptForBlockedRules.map(litArg => s"""${litArg.uid}p -> ${litArg.uid}""")

    // pairs - arguments (that have children) and those children
    val parentNodesTuples = dState.b.filter {
      case arg => arg.parents.nonEmpty  // arguments that have parents
      //case ruleArg: RuleArgument => ruleArg.rule.body.isEmpty // facts
      case _ => false   // TODO: is it necessary?
    }.map(arg =>
      (arg, s"""${arg.uid}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]""")
    )

    val parentEdges3 = remainingBlockedRulesWithHeadsInOpponentRuleArgs.filter(_.parentsIncludingCulprits.nonEmpty)
      .map(ruleArg => s"""{ ${ ruleArg.parentsIncludingCulprits.map(_.uid).mkString(", ") } } -> ${ruleArg.uid}p  """)

    val remainingBlockedRulesParentNodesTuples = remainingBlockedRulesWithHeadsInOpponentRuleArgs
      .filter(_.parentsIncludingCulprits.nonEmpty)
      .map(ruleArg => (ruleArg, s""" ${ruleArg.uid}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ] """) )

    val parentNodes = (parentNodesTuples ++ remainingBlockedRulesParentNodesTuples).map(_._2)

    val parentEdges1 = (parentNodesTuples ++ remainingBlockedRulesParentNodesTuples).map(tuple =>
      s"""${tuple._1.uid + "p"} -> ${tuple._1.uid} """)

    // facts
    val facts = dState.bRuleArgs.filter( _.rule.body.isEmpty )

    // stars
    val factNodes = facts.map ( ruleArg =>
      s""" ${ruleArg.uid}f [ $factAttr, $factShape, fillcolor="${if (dState.pRuleArgs.contains(ruleArg)) getColor(proponentColor) else getColor(opponentColor) }"  ]"""
    )

    // skip the rule argument, when there is an empty body. i.e.
    // for a rule s :- we should obtain
    // ☆ -> ○ -> s
    // instead of
    // ☆ -> ○ -> <s:-> -> ○ -> s
    val parentFactNodes = facts.map ( arg => s"""${LiteralArgument(arg.rule.head).uid}p [ $parentAttr, $parentShape, fillcolor="${getColor(parentColor)}" ]"""  )
    // parent edges for facts
    val factEdges = facts.map(arg => s"${arg.uid}f -> ${LiteralArgument(arg.rule.head).uid}p")

    val parentEdges2 = dState.b.filter(_.parents.exists {
      case _: LiteralArgument => true
      case ruleArg: RuleArgument => ruleArg.rule.body.nonEmpty
    }).map(arg => s"""{ ${arg.parents.filter {
      case _: LiteralArgument => true
      case ruleArg: RuleArgument => ruleArg.rule.body.nonEmpty
    } .map(_.uid).mkString(" ") } } -> ${arg.uid + "p"} """)


    // parent edges from B + culprits to blocked rules
                                                                                  // no need to filter for non empty bodies of parents, since it has to have a body containing a culprit
    // for performance reasons
    lazy val playedBlockedPieces = framework.playedBlockedPieces

    val argumentNodes = dState.b.filter {
      case ruleArg: RuleArgument => ruleArg.rule.body.nonEmpty  // do not draw rule arguments with empty bodies
      case _: LiteralArgument => true
    }.map( arg =>
      s"${arg.uid} [ " +
        s"""label="${arg.toString}", """  +
        s"""fillcolor="${getColor( arg match {
          case litArg: LiteralArgument if framework.goals.contains(litArg.lit) => goalColor
          case litArg: LiteralArgument if defencesArgs.contains(litArg) => defencesColor
          case a if dState.p.contains(a) => proponentColor
          case litArg: LiteralArgument if unattackedOpponentsAssumptions.contains(litArg) => unattackedOpponentsAssumptionsColor
          case a if playedBlockedPieces.contains(a) => blockedColor
          case _ => opponentColor
        })
        }",""" +
        s" ${arg match {
          case _: RuleArgument => ruleShape
          case litArgument: LiteralArgument if framework.assumptions.contains(litArgument.lit) => assumptionShape
          case _ => statementShape
        }}  ] "
    )

    val culpritNodes = framework.culprits.map(LiteralArgument).map(litArg =>
      s""" ${litArg.uid} [ label="${litArg.toString}", $assumptionShape, fillcolor="${getColor(culpritColor)}" ] """
    )

    val propAttacks = (framework.culprits ++ framework.contrariesOf(framework.defences))
      .map(lit =>
        (lit, dState.pLitArgs.filter(litArg => framework.contrariesOf(lit).contains(litArg.lit))))
      .filter(_._2.nonEmpty)
      .map(tuple2 =>
        s""" { ${tuple2._2.map(_.uid).mkString(" ")} } -> ${LiteralArgument(tuple2._1).uid} [ $attackArrowStyle, $propAttackArrowColor ] """)


    val defenceContraries = framework.contrariesOf(framework.defences)
    val playedDefenceContraries = dState.bLitArgs.filter(arg => defenceContraries.contains(arg.lit))

    val oppAttacks = dState.pLitArgs.filter(litArg => framework.defences.contains(litArg.lit))
      .map(litArg =>
        (litArg, playedDefenceContraries.filter(ctr => framework.contrariesOf(litArg.lit).contains(ctr.lit))))
      .filter(_._2.nonEmpty)
      .map(tuple2 =>
        s""" { ${tuple2._2.map(_.uid).mkString(" ")} } -> ${ tuple2._1.uid} [ $attackArrowStyle, $oppAttackArrowColor ] """)

    s"""digraph DisputeStateStep${dState.id} {
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
       |
       |${"\t"}// argument nodes
       |${"\t" + argumentNodes.mkString("\n\t")}
       |${"\t"}// culprit nodes
       |${"\t" + culpritNodes.mkString("\n\t")}
       |
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

}
