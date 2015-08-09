package com.aethericworlds.icfp2015

case class GameState(
  board: Board,
  source: List[Piece],
  piece: Option[Piece] = None,
  prevLines: Int = 0,
  score: Long = 0,
  visited: Set[Piece] = Set.empty[Piece]
) {
  def gameOver = piece == None && (source == Nil || !source.head.enter(board).valid(board))
  def apply(command: Command): GameState = {
    if (gameOver) throw new IllegalArgumentException("Command after game over")
    if (piece == None) {
      val entry = source.head.enter(board)
      copy(piece = Some(entry), source = source.tail, visited = Set(entry))(command)
    } else {
      val moved = command(piece.get)
      if (visited(moved)) throw new IllegalArgumentException("Repeated position in path")
      if (!moved.valid(board)) lock else copy(piece = Some(moved), visited = visited + moved)
    }
  }
  def lock = {
    val middle = board + piece.get
    val lines = (0 until board.height).filter(y => (0 until board.width).forall(x => middle.filled(Cell(x, y)))).toList
    val points = piece.get.members.size + 50 * (lines.size + 1) * lines.size
    val lineBonus = if (prevLines > 1) (prevLines - 1) * points / 10 else 0
    val end = if (lines.isEmpty) middle else
      middle.copy(filled = middle.filled
        .filter { case Cell(x, y) => !(lines contains y) }
        .map { case Cell(x, y) => Cell(x, y + lines.count(_ > y)) }
      )
    GameState(end, source, None, lines.size, score + points + lineBonus)
  }
}

case class Game(input: Input, commands: Traversable[Char], seed: Long, config: Config) {
  val actualCommands = commands.filter{ c => !("\t\n\r".contains(c)) }
  val numUnits = input.units.size
  val pieces = new Source(seed).take(input.sourceLength).map(n => input.units(n % numUnits)).toList
  val pBuf = scala.collection.mutable.ListBuffer[(GameState, List[Char])]()
  var state = GameState(input.board, pieces)
  var used = List.empty[Char]
  val iter = actualCommands.toIterator
  while (iter.nonEmpty && !state.gameOver) {
    val c = iter.next
    try {
      state = state(Command(c))
      used = c :: used
      if (config.debug.contains('@')) System.err.println(state.piece.map(p =>
        p.toString + "pivot: " + p.pivot + "  members: " + p.members + "\n"
      ).getOrElse(" -------- "))
      if (state.piece == None) pBuf += ((state, used))
    } catch {
      case e: IllegalArgumentException if commands.isInstanceOf[Stream[_]] => ()
    }
  }
  val placements = pBuf.toList
  val path = used.reverse.mkString("")
  val remain = actualCommands.drop(path.size)
  if (remain.nonEmpty && !remain.isInstanceOf[Stream[_]])
    throw new IllegalArgumentException(s"didn't consume all commands from ${remain.getClass.getName}, ${remain.size} remaining")
  val moveScore = state.score
  val powerScore = config.phrases.map { phrase =>
    val count = path.sliding(phrase.size).count(_ == phrase)
    if (count > 0) 300 + 2 * phrase.size * count else 0
  }.sum
  def totalScore = moveScore + powerScore
  val output = Output(input.id, seed, config.tag, path)
}
