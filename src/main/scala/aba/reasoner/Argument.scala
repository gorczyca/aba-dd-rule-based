package aba.reasoner

abstract class Argument {

}

case class LiteralArgument() extends Argument
case class RuleArgument() extends Argument

