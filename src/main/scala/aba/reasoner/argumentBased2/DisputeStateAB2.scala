package aba.reasoner.argumentBased2

// import scala.collection.immutable.Stack - do not use, deprecated
// use list instead

import aba.framework.{Framework, Literal, Rule}
import scalaz.{Cord, Show}

import scala.annotation.tailrec





object DisputeStateAB2 {

  import scalaz.Tree
  import scalaz.syntax.Ops
  import scalaz.Scalaz.ToTreeOps


//  trait TreeV[A] extends Ops[A] {
//    def node(subForest: Tree[A]*): Tree[A] = Tree.node(self, subForest.toStream)
//
//    def leaf: Tree[A] = Tree.leaf(self)
//  }
//
//  def tree(framework: Framework): Unit = {
//    val xTree: Tree[Int] =
//      1.node(
//        2.leaf,
//        3.node(
//          4.leaf,
//          5.leaf))
//
//    val asd = xTree.rootLabel
//    val x = xTree.loc.isLeaf
//    val z = xTree.loc.copy().toTree
//    val r = xTree.loc.clone()
//
//
//    val f = 123


//  }

  // funkcja create arguments - taka sama dla proponenta i opponenta, ale po prostu dla proponenta nie bedzie
  // branchowac

  // klasa argument
  // - dostep do uzytych regul globalnie
  // - dostep do uzytych regul w danym branchu
  // - dostep do endpointow (premisow)
  // te 2 rzeczy na koncu:
  // dictionary endpoint: set[Rules]


  // nie ma loopow, wiec tail-recursion

  // 1. tworzymy empty stack z goalami dla prop (negations of defences for opp)
  // 2. wywolujemy funkcje rekurencyjna create_arguments

  // funkcja rekurencyjna create_arguments(stack, complete_args)
  // 1.if stack is empty -> koniec, return complete_args
  //   else wez pierwszy element Arg ze stacku
  //    czy endpointy są puste?
  //      tak -> dodaj Arg do complete args
  //      wywołaj znowu funkcje (ze zmniejszonym stackiem)
  //      nie: (wszystko co poniezej ale tym razem bez wcięć)
  //    wez pierwszy endpoint s
  //      czy s był headem uzytej reguly w tym branchu?
  //        tak -> zaznacz ze infinity i koniec
  //        nie:
  //          czy s był headem uzytej reguły w innych branchach
  //          tak -> uzyj tej samej reguły co była uzyta w innych branchach
  //          nie:
  //            znajdz wszystkie reguly r z headem = s
  //            utworz nowe argumenty dla kazdej reguly r (poprzez dalsza backward expansion) i dodaj je wszystkie do stacku
  //     wywolaj funkcje znowu

  def create_arguments(goals: Set[Literal], rules: Set[Rule])(implicit framework: Framework): Set[ArgumentTree] = {



    @tailrec
    def create_args_rec(stack: List[ArgumentTree], completedArguments: Set[ArgumentTree]): Set[ArgumentTree] = {
      // add to "completed" the actual complete arguments (without any endpoints OR those that have endpoints that we don't have rules for)
      val (actualCompleteArguments, uncompleteArguments) = stack.partition(_.isComplete)
      val (circularArguments, uncircularArguments) = uncompleteArguments.partition(_.isCircular)
      val (furtherUnexpandable, furtherExpandable) = uncircularArguments.partition(_.endpoints.forall{ case (argNode, endpointRules) => !rules.exists(_.head == argNode.data) })

      val currentStack = furtherExpandable
      val newComplete = completedArguments ++ actualCompleteArguments ++ circularArguments ++ furtherUnexpandable

      if (currentStack.isEmpty)
        newComplete
      else {

        val poppedArg::stackAfter = currentStack
        val poppedArgEndpointLit = poppedArg.endpoints.filter(endpoint => rules.exists(_.head == endpoint._1.data)).head._1.data
        val rulesToUse = rules.filter(_.head == poppedArgEndpointLit)

        // val poppedArgEndpoints = poppedArg.endpoints.filter(_._1.data == poppedArgEndpointLit)
        // TODO: mark situation when has no children but rule has no body!!! JUST some token meaning that its empty
        val extendedArgs = rulesToUse.map(rule => poppedArg.dCopy(rule))
        //val uncomplete = extendedArgs -- complete

        val newStack = stackAfter ++ extendedArgs

        create_args_rec(newStack, newComplete)
      }
    }

    val initialStack = goals.map(lit => new ArgumentTree(new ArgumentNode(lit))).toList

    create_args_rec(initialStack, Set.empty[ArgumentTree])
  }


}
