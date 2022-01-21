package dot

import aba.framework.Framework
import aba.reasoner.DisputeState
import aba.reasoner.argumentBased2.{ArgumentNode, ArgumentTree}

import java.io.PrintWriter


object ABDotConverter {

  private val proponentColor = "green"
  private val opponentColor = "yellow"
  private val circularColor = "deeppink"
  private val completeArgColor = "grey"
  private val incompleteArgColor = "gray93"
  private val circularArgColor = "pink"

  private val attackEdgeDefaults = """[fillcolor="white", arrowhead="onormal", color="red:white:red"]"""


  private val proponentGoalShape = "invtriangle"
  private val opponentGoalShape = "triangle"

  private val assumptionShape = "diamond"
  private val factShape = "star"
  private val multipleBodyShape = "circle"

  def exportDotRepr(proponentArgs: Set[ArgumentTree], opponentArgs: Set[ArgumentTree], outputName: String)(implicit dState: DisputeState, framework: Framework): String = {
    val reprString = getDotString(proponentArgs, opponentArgs)
    new PrintWriter(outputName) { write(reprString); close() }

    outputName
  }

  private def getDotString(proponentArgs: Set[ArgumentTree], opponentArgs: Set[ArgumentTree])(implicit dState: DisputeState, framework: Framework): String = {

    val propArgs = proponentArgs.map(argTree => getArgSubgraph(argTree, isProp = true))
    val oppArgs = opponentArgs.map(argTree => getArgSubgraph(argTree, isProp = false))


    // add attacks between arguments
    val allNodeArgsFlattened = (proponentArgs union opponentArgs).flatMap(_.root.flattenTreeSet)
    val attackEdges = framework.contraries.flatMap(contrary => {
      val attackFrom = allNodeArgsFlattened.filter(_.data == contrary.contrary)
      val attackTo = allNodeArgsFlattened.filter(_.data == contrary.assumption)

      for (from <- attackFrom; to <- attackTo) yield s"${from.uid} -> ${to.uid} $attackEdgeDefaults"
    })

    s"""
       |digraph DisputeStateStep${dState.id} {\n
       |\t // global defaults
       |\t 	node [ fontname="times-bold", color="black", style="filled", fontsize="20", margin="0.1,0.0", shape="rectangle"]
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

    val splits = "((?<=[A-Z])|(?=[A-Z]))".r.split(argNode.data.id).toList
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
    val (assumptions, nonAssumtpions) = argumentNodes.partition(argNode => framework.assumptions.contains(argNode.data))
    val (facts, nonFacts) = nonAssumtpions.partition(_.factNode)
    val (goal, nonGoals) = nonFacts.partition(argNode => argNode == argumentTree.root)
    val (circularArgs, nonCircularArgs) = nonGoals.partition(argNode => argumentTree.circularArgs match {
      case Some(circArgs) => circArgs.contains(argNode)
      case _ => false
    })



    // TODO: some function should do it
    val assumptionsNodes = assumptions.map(argNode => s"""${argNode.uid} [label="${getLabel(argNode)}" shape="$assumptionShape"];""")
    val factNodes = facts.map(argNode => s"""${argNode.uid} [label="", shape="$factShape", width="0.15", height="0.15"];""")
    val multipleBodyNodes = getMultipleBodyNode(argumentTree.root).map(uid => s"""$uid [label="", shape="$multipleBodyShape", width=0.15];""")
    val circularNodes = circularArgs.map(argNode => s"""${argNode.uid} [label=\"${getLabel(argNode)}\", color="${circularColor}"];""")
    val goalNodes = goal.map(argNode => s"""${argNode.uid} [label=\"${getLabel(argNode)}\", shape="${if (isProp) proponentGoalShape else opponentGoalShape}"];""") // should be exactly one
    val normalStatements = nonCircularArgs.map(argNode => s"""${argNode.uid} [label="${getLabel(argNode)}"];""")

    val (clusterColor, labelInfo) = if (argumentTree.isCircular) (circularArgColor, "circular") else if (argumentTree.isComplete) (completeArgColor, "complete") else (incompleteArgColor, "incomplete")
    val statementColor = if (isProp) proponentColor else opponentColor
    val label = s"${argumentTree.root.data} ($labelInfo)"

    s"""subgraph cluster_${argumentTree.uid} {
       |\tstyle="filled";
       |\tcolor="$clusterColor";
       |\tlabel="$label";
       |\tfontname="times-bold";
       |\tfontsize="30";
       |\t// node defaults
       |\tnode [color="$statementColor" ]
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
