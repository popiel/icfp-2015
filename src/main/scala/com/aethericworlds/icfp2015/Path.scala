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
      closed += entry
      if (pos.valid(start)) {
        available += entry
        val children = Command.all.map(c => (pos(c), c::how)).filter{case (p, h) => !closed.contains(p)}
        open ++= children
      }
    }
    available
  }
}
