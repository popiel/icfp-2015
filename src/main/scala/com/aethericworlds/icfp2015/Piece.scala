package com.aethericworlds.icfp2015

case class Piece(members: Set[Cell], pivot: Cell) {
  def + (that: Cell) = Piece(members.map(_ + that), pivot + that)
  def - (that: Cell) = this + -that
  def cw = {
    val a = this - pivot
    val b = Piece(a.members.map(_.cw), Cell(0, 0))
    b + pivot
  }
  def ccw = {
    val a = this - pivot
    val b = Piece(a.members.map(_.ccw), Cell(0, 0))
    b + pivot
  }

  def valid(board: Board): Boolean = {
    !members.exists(m => m.x < 0 || m.y < 0 || m.x >= board.width || m.y >= board.height || board.filled(m))
  }

  def enter(board: Board): Piece = {
    val y = members.map(_.y).min
    val lifted = this + Cell(0, -y)
    val minX = lifted.members.map(_.x).min
    val maxX = lifted.members.map(_.x).max
    val x = (board.width - maxX + minX - 1) / 2 - minX
    lifted + Cell(x, 0)
  }

  def apply(c: Command) = c(this)

  override def toString() = {
    val minY = members.map(_.y).min min pivot.y
    val lifted = this + Cell(0, -minY)
    val maxY = lifted.members.map(_.y).max max pivot.y
    val minX = lifted.members.map(_.x).min min pivot.x
    val maxX = lifted.members.map(_.x).max max pivot.x

    (0 to maxY).map { y =>
      (if ((y & 1) == 1) " " else "") +
      (minX to maxX).map { x =>
        val c = Cell(x, y)
        if (c == pivot) {
          if (lifted.members(c)) "P#" else "P]"
        } else {
          if (lifted.members(c)) "##" else "[]"
        }
      }.mkString("") + "\n"
    }.mkString("")
  }
}

sealed trait Command {
  def apply(p: Piece): Piece
}
case object CommandW extends Command { def apply(p: Piece) = p + Cell.W }
case object CommandE extends Command { def apply(p: Piece) = p + Cell.E }
case object CommandSW extends Command { def apply(p: Piece) = p + Cell.SW }
case object CommandSE extends Command { def apply(p: Piece) = p + Cell.SE }
case object CommandCW extends Command { def apply(p: Piece) = p.cw }
case object CommandCCW extends Command { def apply(p: Piece) = p.ccw }

object Command {
  def apply(ch: Char) = {
    val c = ch.toLower
    if ("p'!.03" contains c) CommandW
    else if ("bcefy2" contains c) CommandE
    else if ("aghij4" contains c) CommandSW
    else if ("lmno 5" contains c) CommandSE
    else if ("dqrvz1" contains c) CommandCW
    else if ("kstuwx" contains c) CommandCCW
    else throw new IllegalArgumentException("Bad character " + ch + " in command sequence")
  }

  val all = List(CommandW, CommandE, CommandSW, CommandSE, CommandCW, CommandCCW)
}
