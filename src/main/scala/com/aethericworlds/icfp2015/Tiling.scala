package com.aethericworlds.icfp2015

trait Tiling {
  def place(state: GameState, depth: Int): Seq[Piece] = {
    (for {
      r <- 0 until 6
      if state.source.nonEmpty
      rotated = (0 until r).foldLeft(state.source.head)((p,_) => p.cw)
      y <- 0 until depth
      dropped = rotated + Cell(0, state.board.height - 1 - y - rotated.maxY)
      x <- 0 until state.board.width
      piece = dropped + Cell(x - dropped.minX, 0)
      if Paths.findPath(state.board, state.source.head.enter(state.board), piece) != None
      // if piece.valid(state.board) && piece.lockable(state.board)
    } yield piece).distinct
  }

  def fillBottom(start: GameState, depth: Int): List[Piece] = {
    val pieces = start.source

    def fill(state: GameState, prior: List[Piece]): Seq[(Int, List[Piece])] =
      place(state, depth).flatMap{ p =>
        val next = start(p)
        val lines = (0 until next.board.height).filter(y => (0 until next.board.width).forall(x => next.board.filled(Cell(x, y)))).size
        if (lines > 0) List(lines -> (p :: prior).reverse) else fill(next, p :: prior)
      }

    fill(start, Nil).maxBy(_._1)._2
  }


  def fill2(start: GameState, depth: Int): List[Piece] = {
    implicit val ordering = new Ordering[(GameState, List[Piece])] {
      def compare(a: (GameState, List[Piece]), b: (GameState, List[Piece])) = {
        val c = b._1.board.compareTo(a._1.board)
        val s = a._1.score.compareTo(b._1.score)
        if (s == 0) if (c == 0) a._1.source.size - b._1.source.size else c else s
      }
    }
    val open = new scala.collection.mutable.PriorityQueue[(GameState, List[Piece])]()
    var closed = List[(GameState, Set[Piece])]()
    var best: (GameState, List[Piece]) = (start, Nil)
    open += ((start, Nil))
    while (open.nonEmpty) {
      val entry = open.dequeue
      val (pos, how) = entry
      val set = how.toSet
      if (!closed.exists(x => x._1 == pos || set.subsetOf(x._2))) {
        // println("Working on " + (pos.board, pos.score, how))
        closed = (pos, set) :: closed
        if (pos.score == start.score || pos.prevLines == 0) {
          val wheres = place(pos, depth)
          val children = wheres.map(x => (pos(x), x :: how))
          open ++= children
          val oldBest = best
          best = (best +: children).max
          // if (best != oldBest) println("New Best: " + (best._1.board, best._1.score, best._2))
        }
      }
    }
    best._2.reverse
  }
}

object Tiling extends Tiling
