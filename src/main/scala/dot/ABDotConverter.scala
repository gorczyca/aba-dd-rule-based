package dot

import aba.framework.Framework
import aba.reasoner.{DisputeState, PotentialAssumptionMove, PotentialMove2}
import aba.reasoner.argumentBased2.{ArgumentNode, ArgumentTree, DisputeStateAB2}

import java.io.PrintWriter


object ABDotConverter {

  private val proponentColor = "green"
  private val opponentColor = "yellow"
  private val circularColor = "deeppink"
  private val completeArgColor = "grey"
  private val incompleteArgColor = "gray93"
  private val circularArgColor = "pink"

  private val proponentWonColor = "palegreen"
  private val opponentWonColor = "lightpink"
  private val gameOverFontSize = "100"

  private val attackEdgeDefaults = """[fillcolor="white", arrowhead="onormal", color="red:white:red"]"""

  private val proponentGoalShape = "invtriangle"
  private val opponentGoalShape = "triangle"

  private val assumptionShape = "diamond"
  private val factShape = "star"
  private val multipleBodyShape = "circle"

  def generateABRepresentation(showCircular: Boolean = true,
                               showIncomplete: Boolean = true,
                               showConflicted: Boolean = true,
                               over: Option[Boolean] = None,
                               outputName: String = "temp_arg.dot",
                               additionalInformation: String = "")
                               (implicit dState: DisputeState, framework: Framework, performedMoves: List[PotentialMove2]): String = {

    // assumptions used with PF2 that are not contained in any rule
    val propAssumptionGoals = performedMoves.filter(_.moveType.isProponentMove).collect {
      case asmMove: PotentialAssumptionMove if !dState.pRules.flatMap(_.statements).contains(asmMove.assumption) => asmMove.assumption
    }.toSet

    val actualPropGoals = framework.goals  union framework.contrariesOf(dState.culprits) union propAssumptionGoals

    def filterCircular: ArgumentTree => Boolean = arg => showCircular || !arg.isCircular
    def filterIncomplete: ArgumentTree => Boolean = arg => showIncomplete || arg.isComplete
    def filterConflicted: ArgumentTree => Boolean = arg => showConflicted || !arg.isConflicted

    val propArgs = DisputeStateAB2.create_arguments(actualPropGoals, dState.pRules)
      .filter(filterCircular)
      .filter(filterConflicted)
      .filter(filterIncomplete)

    // maximality
    val propArgs2 = propArgs.filter(arg => !(propArgs - arg).exists(otherArg => arg.rulesUsed.subsetOf(otherArg.rulesUsed)))

    // assumptions used in OF2 moves that are not parts of any rules used
    val oppAssumptionGoals = performedMoves.filter(_.moveType.isOpponentsMove).collect {
      case asmMove: PotentialAssumptionMove if !dState.bRules.flatMap(_.statements).contains(asmMove.assumption) => asmMove.assumption
    }.toSet

    val opponentGoals = (dState.defenceContraries intersect dState.bStatements) union oppAssumptionGoals // those that actually have been uttered
    val oppArgs = DisputeStateAB2.create_arguments(opponentGoals, dState.bRules)
      .filter(filterCircular)
      .filter(filterConflicted)
      .filter(filterIncomplete)

    // maximality
    val oppArgs2 = oppArgs.filter(arg => !(oppArgs - arg).exists(otherArg => arg.rulesUsed.subsetOf(otherArg.rulesUsed)))

    ABDotConverter.exportDotRepr(propArgs2, oppArgs2, outputName, additionalInformation, over)

    outputName
  }


  def exportDotRepr(proponentArgs: Set[ArgumentTree], opponentArgs: Set[ArgumentTree], outputName: String, additionalInformation: String, indicateOver: Option[Boolean])
                   (implicit dState: DisputeState, framework: Framework): String = {
    val reprString = getDotString(proponentArgs, opponentArgs, additionalInformation, indicateOver)
    new PrintWriter(outputName) { write(reprString); close() }

    outputName
  }

  private def getDotString(proponentArgs: Set[ArgumentTree], opponentArgs: Set[ArgumentTree], additionalInformation: String, indicateOver: Option[Boolean])(implicit dState: DisputeState, framework: Framework): String = {

    val propArgs = proponentArgs.map(argTree => getArgSubgraph(argTree, isProp = true))
    val oppArgs = opponentArgs.map(argTree => getArgSubgraph(argTree, isProp = false))


    // add attacks between arguments
    val allNodeArgsFlattened = (proponentArgs union opponentArgs).flatMap(_.root.flattenTreeSet)
    val attackEdges = framework.contraries.flatMap(contrary => {
      val attackFrom = allNodeArgsFlattened.filter(_.statement == contrary.contrary)
      val attackTo = allNodeArgsFlattened.filter(_.statement == contrary.assumption)

      for (from <- attackFrom; to <- attackTo) yield s"${from.uid} -> ${to.uid} $attackEdgeDefaults"
    })


    // TODO: PASS step

    val STEP = 999

    // \tbgcolor="white:${proponentWonColor}";


    var indicateOverInfo = indicateOver match {
      case Some(true) =>
        s"""
           |\tbgcolor="${proponentWonColor}";
           |\tlabel="Proponent won.";
           |\tfontname="times-italic";
           |\tfontsize="${gameOverFontSize}";
           |""".stripMargin
      case Some(false) =>
        s"""
           |\tbgcolor="${opponentWonColor}";
           |\tlabel="Opponent won.";
           |\tfontname="times-italic";
           |\tfontsize="${gameOverFontSize}";
           |""".stripMargin
      case None => ""
    }

    s"""
       |${additionalInformation}
       |
       |digraph DisputeStateStep${STEP} {\n
       |
       |$indicateOverInfo
       |
       |
       |\t // global defaults
       |\t 	node [ fontname="times-bold", color="black", style="filled", fontsize="20", margin="0.1,0.0", shape="rectangle"]
       |\t  rankdir="BT";
       |\n
       |// Proponent's args
       |${propArgs.mkString("\n\n")}
       |\n\n\n
       |// Opponent's args
       |${oppArgs.mkString("\n\n")}
       |\n\n\n
       |// Attacks between args
       |${attackEdges.mkString("\n\t")}
       |}""".stripMargin

  }

  def getLabel(argNode: ArgumentNode): String = {

    val maxLength = 3

    val splits = "((?<=[A-Z])|(?=[A-Z]))".r.split(argNode.statement).toList
    val firstElem::tail = splits

    val (newTailOdd, newTailEven) = tail.zipWithIndex.partition{ case (_, index) => index % 2 ==0 }
    val actualWords = (newTailOdd.map(_._1) zip newTailEven.map(_._1)).map(pair => pair._1 + pair._2)


    // TODO: add option to hide arguments that are incomplete and cannot be complete

    val output = actualWords.foldLeft(List[String](firstElem)) { (accumulator, splitElement) =>
      if (accumulator.last.length > maxLength) {
        accumulator :+ splitElement
      } else {
        val lastElem = accumulator.last

        accumulator.dropRight(1) :+ lastElem + splitElement
      }
    }.mkString("\n")

    output

  }

  private def getArgSubgraph(argumentTree: ArgumentTree, isProp: Boolean)(implicit framework: Framework): String = {

    //@tailrec TODO: not tail recursive
    def getArgsEdgesRec(argumentNode: ArgumentNode): Set[String] = {
      if (argumentNode.children.isEmpty) {
        Set.empty[String]
      } else if (argumentNode.children.size == 1) {
        val onlyChild = argumentNode.children.head
        val edge = s"${onlyChild.uid} -> ${argumentNode.uid};"
        Set(edge) ++ getArgsEdgesRec(onlyChild)
      } else {
        val additionalNodeUid = argumentNode.uid + "_children"
        val edge = s"${additionalNodeUid} -> ${argumentNode.uid};"
        val additionalEdges = argumentNode.children.map(argNode => s"${argNode.uid} -> ${additionalNodeUid};")
        Set(edge) ++ additionalEdges ++ argumentNode.children.flatMap(getArgsEdgesRec)
      }
    }

    def getMultipleBodyNode(argumentNode: ArgumentNode): Set[String] = {
      if (argumentNode.children.isEmpty) {
        Set.empty[String]
      }
      else if (argumentNode.children.size == 1) {
        argumentNode.children.flatMap(getMultipleBodyNode)
      } else {
        Set(argumentNode.uid + "_children") ++ argumentNode.children.flatMap(getMultipleBodyNode)
      }
    }

    val argumentNodes = argumentTree.root.flattenTree
    val (assumptions, nonAssumtpions) = argumentNodes.partition(argNode => framework.assumptions.contains(argNode.statement))
    val (facts, nonFacts) = nonAssumtpions.partition(_.factNode)
    val (goal, nonGoals) = nonFacts.partition(argNode => argNode == argumentTree.root)
    val (circularArgs, nonCircularArgs) = nonGoals.partition(argNode => argumentTree.circularArgs match {
      case Some(circArgs) => circArgs.contains(argNode)
      case _ => false
    })

    val statementColor = if (isProp) proponentColor else opponentColor


    // TODO: some function should do it
    val assumptionsNodes = assumptions.map(argNode => s"""${argNode.uid} [label="${getLabel(argNode)}", shape="$assumptionShape", fillcolor="white:$statementColor"];""")
    val factNodes = facts.map(argNode => s"""${argNode.uid} [label="", shape="$factShape", width="0.15", height="0.15", fillcolor="white:$statementColor"];""")
    val multipleBodyNodes = getMultipleBodyNode(argumentTree.root).map(uid => s"""$uid [label="", shape="$multipleBodyShape", width=0.15, fillcolor="white:$statementColor"];""")
    val circularNodes = circularArgs.map(argNode => s"""${argNode.uid} [label="${getLabel(argNode)}", fillcolor="white:$circularColor"];""")
    val goalNodes = goal.map(argNode => s"""${argNode.uid} [label="${getLabel(argNode)}", shape="${if (isProp) proponentGoalShape else opponentGoalShape}", fillcolor="white:$statementColor"]; """) // should be exactly one
    val normalStatements = nonCircularArgs.map(argNode => s"""${argNode.uid} [label="${getLabel(argNode)}", fillcolor="white:$statementColor"];""")

    val (clusterColor, labelInfo) = if (argumentTree.isCircular) (circularArgColor, "circular") else if (argumentTree.isComplete) (completeArgColor, "complete") else (incompleteArgColor, "incomplete")
    val label = s"${argumentTree.root.statement} ($labelInfo)"

    s"""subgraph cluster_${argumentTree.uid} {
       |\tstyle="filled";
       |\tcolor="black";
       |\tfillcolor="$clusterColor";
       |\tlabel="$label";
       |\tfontname="times-italic";
       |\tfontsize="30";
       |\t// node defaults
       |\tnode [color="black" ];
       |\t// edges
       |\t${getArgsEdgesRec(argumentTree.root).mkString("\n\t")}
       |\n
       |\t// nodes
       |\t// assumptions
       |\t${assumptionsNodes.mkString("\n\t")}
       |\t// facts
       |\t${factNodes.mkString("\n\t")}
       |\t// multipleBodyNodes
       |\t${multipleBodyNodes.mkString("\n\t")}
       |\t// circularNodes
       |\t${circularNodes.mkString("\n\t")}
       |\t// goalNodes
       |\t${goalNodes.mkString("\n\t")}
       |\t// normalStatements
       |\t${normalStatements.mkString("\n\t")}
       |}""".stripMargin
  }
}
