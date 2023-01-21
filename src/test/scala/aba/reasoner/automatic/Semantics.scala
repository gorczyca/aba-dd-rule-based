package aba.reasoner.automatic

// TODO: make use of this in the project
object Semantics extends Enumeration {
  type Semantics = Value

  val Admissible, Complete, Stable, Grounded, Preferred = Value
}