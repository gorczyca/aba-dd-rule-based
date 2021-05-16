package aba.move

class PF1Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("PF1 move")
    100
  }}
