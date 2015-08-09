package com.aethericworlds.icfp2015

case class Placement(start: Board, piece: Piece, commands: Traversable[Command]) {
  val positions = {
    var pos = piece.enter(start) :: Nil
    val iter = commands.toIterator
    while (pos.head.valid(start) && iter.hasNext) pos = iter.next.apply(pos.head) :: pos
    if (pos.head.valid(start)) pos else pos.tail
  }
  val path = commands.take(positions.size)
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
  val actualCommands = commands.filter{ c => !("\t\n\r".contains(c)) }
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
  if (remain.nonEmpty && !remain.isInstanceOf[Stream[Command]])
    throw new IllegalArgumentException(s"didn't consume all commands from ${remain.getClass.getName}, ${remain.size} remaining")
  val placements = pBuf.toList
  val path = actualCommands.take(placements.map(_.path.size).sum).mkString("")
  val moveScore = placements.map(_.points).sum + placements.sliding(2).map(l => l(1).lineBonus(l(0).lines.size)).sum
  val powerScore = config.phrases.map { phrase =>
    val count = path.sliding(phrase.size).count(_ == phrase)
    if (count > 0) 300 + 2 * phrase.size * count else 0
  }.sum
  def totalScore = moveScore + powerScore
  val output = Output(input.id, seed, config.tag, path)
}
