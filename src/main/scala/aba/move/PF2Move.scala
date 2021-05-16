package aba.move

class PF2Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("PF2 move")
    100
  }}
