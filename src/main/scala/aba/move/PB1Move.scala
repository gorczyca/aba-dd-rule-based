package aba.move

class PB1Move extends Move {
  override def isPossible(dummy: Int): Boolean = true

  override def perform(dummy: Int): Int = {
    println("PB1 move")
    100
  }}
