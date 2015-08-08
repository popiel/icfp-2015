package com.aethericworlds.icfp2015

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}

object Main extends Coordinator {
  def main(args: Array[String]) {
    val config = parseArgs(args)
    println(config)
  }
}

class Coordinator {
  def parseArgs(args: Array[String]): Config = {
    args.grouped(2).foldLeft(Config()) { (c, l) => (l(0), l(1)) match {
      case ("-f", file)   => c.copy(files = c.files :+ file)
      case ("-t", num)    => c.copy(timeLimit = Some(num.toInt))
      case ("-m", num)    => c.copy(memoryLimit = Some(num.toInt))
      case ("-p", phrase) => c.copy(phrases = c.phrases :+ phrase.toLowerCase)
      case (opt, value)   => throw new IllegalArgumentException(s"Unrecognized option '$opt'")
    } }
  }

  implicit val formats = Serialization.formats(NoTypeHints)

  def loadInputs(files: List[String]): List[Input] =
    files.map { file => read[Input](io.Source.fromFile(file).getLines.mkString("\n")) }

  def formatOutputs(stuff: List[Output]) = write(stuff)
}

case class Config(files: List[String] = Nil, timeLimit: Option[Int] = None, memoryLimit: Option[Int] = None, phrases: List[String] = Nil)

case class Input(id: JValue, units: List[Piece], width: Int, height: Int, filled: List[Cell], sourceLength: Int, sourceSeeds: List[Long]) {
  val board = Board(width = width, height = height, filled = filled.toSet)
}
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

// Thanks go to http://www.redblobgames.com/grids/hexagons/#rotation for this representation.
case class Cube(x: Int, y: Int, z: Int) {
  def toCell = Cell(x + (z - (z&1)) / 2, z)
  def cw = Cube(-z, -x, -y)
  def ccw = Cube(-y, -z, -x)
  def unary_- = Cube(-x, -y, -z)
  def + (that: Cube) = Cube(this.x + that.x, this.y + that.y, this.z + that.z)
  def - (that: Cube) = Cube(this.x - that.x, this.y - that.y, this.z - that.z)
}
case class Cell(x: Int, y: Int) {
  def toCube = {
    val xx = x - (y - (y&1)) / 2
    val zz = y
    Cube(xx, -xx-zz, zz)
  }

  def e = Cell(x + 1, y)
  def w = Cell(x - 1, y)
  def se = Cell(x + (y&1), y + 1)
  def sw = Cell(x - 1 + (y&1), y + 1)
  def ne = Cell(x + (y&1), y - 1)
  def nw = Cell(x - 1 + (y&1), y - 1)

  def cw = toCube.cw.toCell
  def ccw = toCube.ccw.toCell

  def unary_- = toCube.unary_-.toCell
  def + (that: Cell) = Cell(this.x + that.x + (this.y & that.y & 1), this.y + that.y)
  def - (that: Cell) = this + -that
}
object Cell {
  val E = Cell(1, 0)
  val W = Cell(-1, 0)
  val SE = Cell(0, 1)
  val SW = Cell(-1, 1)
  val NE = Cell(0, -1)
  val NW = Cell(-1, -1)

  val DIRS = List(E, W, SE, SW)
  val ALL_DIRS = List(E, W, SE, SW, NE, NW)
}

case class Board(width: Int, height: Int, filled: Set[Cell]) {
  override def toString = {
    (0 until height).map { y =>
      (if (y % 2 == 1) " " else "") +
      (0 until width).map { x =>
        if (filled(Cell(x, y))) "##" else "[]"
      }.mkString("") + "\n"
    }.mkString("")
  }

  def + (unit: Piece) = copy(filled = filled ++ unit.members)
}

case class Output(problemId: JValue, seed: Long, tag: String, solution: String)

class Source(var seed: Long) extends Iterator[Int] {
  def next = {
    val output = (seed >> 16) & 0x7fff
    seed = (seed * 1103515245 + 12345) & 0xffffffff
    output.toInt
  }
  def hasNext = true
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
  def apply(c: Char) = {
    if ("p'!.03" contains c) CommandW
    else if ("bcefy2" contains c) CommandE
    else if ("aghij4" contains c) CommandSW
    else if ("lmno 5" contains c) CommandSE
    else if ("dqrvz1" contains c) CommandCW
    else if ("kstuwx" contains c) CommandCCW
    else throw new IllegalArgumentException("Bad character " + c + " in command sequence")
  }
}

case class Placement(start: Board, piece: Piece, commands: Traversable[Command]) {
  val (positions, path) = {
    var pos = piece.enter(start) :: Nil
    val iter = commands.toIterator
    while (pos.head.valid(start) && iter.hasNext) pos = iter.next.apply(pos.head) :: pos
    (pos.tail, commands.take((pos.size - 1) max 0))
  }
  require(positions.size == positions.toSet.size, "Repeated positions in path")

  def remaining = commands.drop(path.size)

  val middle = if (positions.nonEmpty) start + positions.head else start

  val lines = (0 until start.height).filter(y => (0 until start.width).forall(x => middle.filled(Cell(x, y)))).toList
  val points = if (positions.nonEmpty) piece.members.size + 50 * (lines.size + 1) * lines.size else 0
  def lineBonus(prevLines: Int) = if (prevLines > 1) (prevLines - 1) * points / 10 else 0
  val end = if (lines.isEmpty) middle else
    middle.copy(filled = middle.filled
      .filter { case Cell(x, y) => !(lines contains y) }
      .map { case Cell(x, y) => Cell(x, y + lines.count(_ > y)) }
    )
}

case class Game(input: Input, commands: Traversable[Char], seed: Long, config: Config) {
  val actualCommands = commands.filter{ c => !("\t\n\r".contains(c)) }.map(_.toLower)
  val numUnits = input.units.size
  val pieces = new Source(seed).take(input.sourceLength).map(n => input.units(n % numUnits)).toList
  val pBuf = scala.collection.mutable.ListBuffer[Placement]()
  var remain: Traversable[Command] = actualCommands.map(Command(_))
  var board = input.board
  pieces.foreach { piece =>
    val place = Placement(board, piece, remain)
    pBuf += place
    remain = place.remaining
    board = place.end
  }
  val placements = pBuf.toList
  val path = actualCommands.take(placements.map(_.path.size).sum).mkString("")
  val moveScore = placements.map(_.points).sum + placements.sliding(2).map(l => l(1).lineBonus(l(0).lines.size)).sum
  val powerScore = config.phrases.map { phrase =>
    val count = path.sliding(phrase.size).count(_ == phrase)
    if (count > 0) 300 + 2 * phrase.size * count else 0
  }.sum
  def totalScore = moveScore + powerScore
  val output = Output(input.id, seed, "", path)
}
