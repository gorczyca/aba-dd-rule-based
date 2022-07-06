package interface

object CustomImplicits {

  // TODO: move to implicits
  implicit class mapWrapper[K, V](map: Map[K, Seq[V]]) {
    def randomElement: V = {
      val n = util.Random.nextInt(map.size)
      val seq = map.iterator.drop(n).next._2 // entry [MoveType, Seq[PotentialMoves]] -> now take random PotentialMove
      val m = util.Random.nextInt(seq.size)
      seq.iterator.drop(m).next
    }
  }

  implicit class listWrapper[T](list: List[T]) {
    def safeBacktrack(n: Int = 1): List[T] = list.take(math.max(list.size - n, 1))
  }


  implicit class seqWrapper[T](seq: Seq[T]) {
    def random: T = {
      val n = util.Random.nextInt(seq.size)
      seq.iterator.drop(n).next
    }
  }
}