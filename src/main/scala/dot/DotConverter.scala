package dot

import aba.framework.Framework
import aba.reasoner.{DisputeState, LiteralArgument, RuleArgument}

import java.io.PrintWriter

object DotConverter {
  def exportDotRepr(outputFileNameOpt: Option[String] = None)(implicit dState: DisputeState, framework: Framework): String = {
    val reprString = getDotString

    val outputFileName = outputFileNameOpt match {
      case Some(name) => name
      case _ => s"dispute_step${dState.id}.dot"
    }

    new PrintWriter(outputFileName) { write(reprString); close() }

    outputFileName
  }

  private def getDotString(implicit dState: DisputeState, framework: Framework): String = {

    val nodeDefaults = s"""node [fontname="times-bold", color="black", style="filled"]  """

    val parentAttr = """ label="", shape="circle", fillcolor="yellow", width=0.3 """
    val ruleShape = """ shape="hexagon" """
    val factShape = """label="", shape="star", width=0.5"""
    val assumptionShape = """shape="diamond", width=0.75, height=0.75"""
    val statementShape = """shape="square", width=0.6"""
    val proponentColor = """fillcolor="green" """
    val opponentColor = """fillcolor="yellow" """
    val culpritColor = """fillcolor="red" """
    val blockedColor = """fillcolor="orange" """

    val attackArrowStyle = """ fillcolor="white", arrowhead="onormal" """
    val propAttackArrowColor = """ color="black:white:black" """
    val oppAttackArrowColor = """ color="red:white:red"  """

    val parentNodesTuples = dState.b.filter {
      case arg => arg.parents.nonEmpty  // arguments that have parents
      //case ruleArg: RuleArgument => ruleArg.rule.body.isEmpty // facts
      case _ => false   // TODO: is it necessary?
    }.map(arg =>
      (arg, s"""${arg.uid}p [ $parentAttr ]""")
    )

    val parentNodes = parentNodesTuples.map(_._2)

    val parentEdges1 = parentNodesTuples.map(tuple =>
      s""" ${tuple._1.uid + "p"}  -> ${tuple._1.uid} """)

    // facts
    val facts = dState.bRuleArgs.filter( _.rule.body.isEmpty )

    // stars
    val factNodes = facts.map ( ruleArg =>
      s""" ${ruleArg.uid}f [ $factShape, ${if (dState.pRuleArgs.contains(ruleArg)) proponentColor else opponentColor }  ]"""
    )

    // skip the rule argument, when there is an empty body. i.e.
    // for a rule s :- we should obtain
    // ☆ -> ○ -> s
    // instead of
    // ☆ -> ○ -> <s:-> -> ○ -> s
    val parentFactNodes = facts.map ( arg => s"""${LiteralArgument(arg.rule.head).uid}p [ $parentAttr ]"""  )
    // parent edges for facts
    val factEdges = facts.map(arg => s"${arg.uid}f -> ${LiteralArgument(arg.rule.head).uid}p")


    val parentEdges2 = dState.b.filter(_.parents.exists {
      case _: LiteralArgument => true
      case ruleArg: RuleArgument => ruleArg.rule.body.nonEmpty
    }).map(arg => s""" { ${arg.parents.map(_.uid).mkString(" ") } } -> ${arg.uid + "p"} """)

    // for performance reasons
    lazy val playedBlockedPieces = framework.playedBlockedPieces

    val argumentNodes = dState.b.filter {
      case ruleArg: RuleArgument => ruleArg.rule.body.nonEmpty  // do not draw rule arguments with empty bodies
      case _: LiteralArgument => true
    }.map( arg =>
      s"${arg.uid} [ " +
        s"""label="${arg.toString}", """  +
        s"${ if (dState.p.contains(arg)) proponentColor else if (playedBlockedPieces.contains(arg)) blockedColor else opponentColor }," +
        s" ${arg match {
          case _: RuleArgument => ruleShape
          case litArgument: LiteralArgument if framework.assumptions.contains(litArgument.lit) => assumptionShape
          case _ => statementShape
        }}  ] "
    )

    val culpritNodes = framework.culprits.map(LiteralArgument).map(litArg =>
      s""" ${litArg.uid} [ label="${litArg.toString}", $assumptionShape, $culpritColor ] """
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
       |
       |${"\t"}// fact nodes
       |${"\t" + factNodes.mkString("\n\t")}
       |${"\t" + parentFactNodes.mkString("\n\t")}
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
