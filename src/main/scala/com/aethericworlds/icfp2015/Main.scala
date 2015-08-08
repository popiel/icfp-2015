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

case class Input(id: JValue, units: List[Piece], width: Int, height: Int, filled: List[Cell], sourceLength: Int, sourceSeeds: List[Long])
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
  def valid(implicit board: Board): Boolean = {
    !members.exists(m => m.x < 0 || m.y < 0 || m.x >= board.width || m.y >= board.height || board.filled(m))
  }
  def enter(implicit board: Board): Piece = {
    val y = members.map(_.y).min
    val lifted = this + Cell(0, -y)
    val minX = lifted.members.map(_.x).min
    val maxX = lifted.members.map(_.x).max
    val x = (board.width - maxX + minX) / 2 - minX
    lifted + Cell(x, 0)
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

case class Board(width: Int, height: Int, filled: Set[Cell], sourceLength: Int, sourceSeed: Long, score: Long) {
  override def toString = {
    (0 until height).map { y =>
      (if (y % 2 == 1) " " else "") +
      (0 until width).map { x =>
        if (filled(Cell(x, y))) "##" else "[]"
      }.mkString("") + "\n"
    }.mkString("") + s"Score: $score, Length: $sourceLength, Seed: $sourceSeed\n"
  }

  def + (unit: Piece) = {
    val locked = copy(filled = filled ++ unit.members)
  }
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
