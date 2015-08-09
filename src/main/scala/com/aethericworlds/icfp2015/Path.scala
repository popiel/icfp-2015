package com.aethericworlds.icfp2015

case class Paths(start: Board, piece: Piece) {
  val initial = piece.enter(start)
  val reachable: Map[Piece, List[Command]] = {
    val open = collection.mutable.Queue[(Piece, List[Command])]()
    var closed = Map[Piece, List[Command]]()
    var available = Map[Piece, List[Command]]()
    if (initial.valid(start)) open += ((initial, Nil))
    while (open.nonEmpty) {
      val entry = open.dequeue
      val (pos, how) = entry
      if (!closed.contains(pos)) {
        closed += entry
        val children = Command.all.map(c => (pos(c), c::how))
        val (viable, bad) = children.partition(_._1.valid(start))
        if (bad.nonEmpty) available += ((pos, bad.head._2.reverse))
        open ++= viable.filter(x => !closed.contains(x._1))
      }
    }
    available
  }
}

object Paths {
  def findBest(start: GameState, depth: Int): (GameState, List[Command]) = {
    if (depth == 0 || start.gameOver) (start, Nil)
    else {
      val paths = Paths(start.board, start.source.head)
      val children = paths.reachable.toList map { case (piece, path) =>
        val placed = start(piece)
        val nest = findBest(placed, depth - 1)
        (nest._1, path)
      }
      children.sortWith { (a, b) =>
        if (a._1.score > b._1.score) true
        else if (a._1.score < b._1.score) false
        else a._1.board < b._1.board
      }.head
    }
  }
}
