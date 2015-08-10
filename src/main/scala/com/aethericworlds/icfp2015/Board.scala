package com.aethericworlds.icfp2015

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

case class Board(width: Int, height: Int, filled: Set[Cell]) extends Ordered[Board] {
  override def toString = {
    (0 until height).map { y =>
      (if (y % 2 == 1) " " else "") +
      (0 until width).map { x =>
        if (filled(Cell(x, y))) "##" else "[]"
      }.mkString("") + "\n"
    }.mkString("")
  }

  def + (unit: Piece) = copy(filled = filled ++ unit.members)

  val sumX = filled.toList.map(_.x).sum
  val sumY = filled.toList.map(_.y).sum

  def compare(that: Board) = {
    val height = that.sumY - this.sumY
    if (height == 0) this.sumX - that.sumX else height
  }
}
