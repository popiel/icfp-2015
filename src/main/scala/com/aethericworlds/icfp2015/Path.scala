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

  def findPath(board: Board, start: Piece, end: Piece): Option[List[Command]] = {
    if (!start.valid(board) || !end.valid(board) || !end.lockable(board)) return None

    val sum = Cell(end.members.toIterator.map(_.x).sum,
                      end.members.toIterator.map(_.y).sum)
    implicit val ordering = new Ordering[(Piece, List[Command])] {
      def compare(a: (Piece, List[Command]), b: (Piece, List[Command])) = {
        val aDist = a._1.members.toIterator.map(x => x.x - sum.x + x.y - sum.y).sum
        val bDist = b._1.members.toIterator.map(x => x.x - sum.x + x.y - sum.y).sum
        bDist - aDist
      }
    }
    val open = new scala.collection.mutable.PriorityQueue[(Piece, List[Command])]()
    var closed = Map[Piece, List[Command]]()
    open += ((start, Nil))
    while (open.nonEmpty) {
      val entry = open.dequeue
      val (pos, how) = entry
      if (!closed.contains(pos)) {
        closed += entry
        val children = Command.all.map(c => (pos(c), c::how))
        val (viable, bad) = children.partition(_._1.valid(board))
        if (bad.nonEmpty && pos == end) return Some(bad.head._2.reverse)
        open ++= viable.filter(x => !closed.contains(x._1))
      }
    }
    return None
  }

  def findPowerPath(board: Board, start: Piece, end: Piece, config: Config): Option[String] = {
    if (!start.valid(board) || !end.valid(board) || !end.lockable(board)) return None

    val sum = Cell(end.members.toIterator.map(_.x).sum,
                      end.members.toIterator.map(_.y).sum)
    implicit val ordering = new Ordering[(Piece, List[Command])] {
      def compare(a: (Piece, List[Command]), b: (Piece, List[Command])) = {
        val aDist = a._1.members.toIterator.map(x => x.x - sum.x + x.y - sum.y).sum
        val bDist = b._1.members.toIterator.map(x => x.x - sum.x + x.y - sum.y).sum
        bDist - aDist
      }
    }
    val open = new scala.collection.mutable.PriorityQueue[(Piece, String)]()
    var closed = Map[Piece, String]()
    open += ((start, ""))
    while (open.nonEmpty) {
      val entry = open.dequeue
      val (pos, how) = entry
      if (!closed.contains(pos)) {
        closed += entry
        val simple = Command.all.map(c => (pos(c), how + c))
	val complex = config.phrases.flatMap { phrase =>
          Try {
            val chunk = how + phrase
            val next = GameState(board, List(start), Some(start))(chunk)
            List((next, chunk))
          }.getOrElse(Nil)
        }
        val children = simple + complex
        val (viable, bad) = children.partition(_._1.valid(board))
        if (bad.nonEmpty && pos == end) return Some(bad.head._2)
        open ++= viable.filter(x => !closed.contains(x._1))
      }
    }
    return None
  }
}
