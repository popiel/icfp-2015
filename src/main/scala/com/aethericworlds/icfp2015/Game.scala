package com.aethericworlds.icfp2015

import scala.annotation.tailrec

case class GameState(
  board: Board,
  source: List[Piece],
  piece: Option[Piece] = None,
  prevLines: Int = 0,
  score: Long = 0,
  visited: Set[Piece] = Set.empty[Piece]
) {
  def this(input: Input, seed: Long) =
    this(input.board, new Source(seed).take(input.sourceLength).map(n => input.units(n % input.units.size)).toList)

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

  def apply(path: Traversable[Command]): GameState = (this /: path){ (s, c) => s(c) }

  def apply(target: Piece): GameState = {
    if (piece != None) throw new IllegalStateException("May not jump while falling")
    if (!target.valid(board)) throw new IllegalArgumentException("May not jump into a wall")
    val normal = source.head.enter(board)
    if (normal != target.enter(board) &&
        normal != target.cw.enter(board) &&
        normal != target.cw.cw.enter(board) &&
        normal != target.cw.cw.cw.enter(board) &&
        normal != target.cw.cw.cw.cw.enter(board) &&
        normal != target.cw.cw.cw.cw.cw.enter(board))
      throw new IllegalArgumentException("May not skip pieces")
    copy(piece = Some(target), source = source.tail).lock
  }
}

trait GameInfo {
  def config: Config
  def output: Output
  def pieces: List[Piece]
  def moveScore: Long
  lazy val powerScore = config.phrases.map { phrase =>
    @tailrec def find(from: Int, seen: Int): Int = {
      val pos = output.solution.indexOf(phrase, from)
      if (pos < 0) seen else find(pos + 1, seen + 1)
    }
    val count = find(0, 0)
    if (count > 0) 300 + 2 * phrase.size * count else 0
  }.sum
  def totalScore = moveScore + powerScore
}

case class Game(input: Input, commands: Traversable[Char], seed: Long, config: Config) extends GameInfo {
  var state = new GameState(input, seed)
  val pieces = state.source

  val actualCommands = commands.filter{ c => !("\t\n\r".contains(c)) }
  val pBuf = scala.collection.mutable.ListBuffer[(GameState, List[Char])]()
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
  val output = Output(input.id, seed, config.tag, path)
  val moveScore = state.score
}

case class GameSearch(input: Input, seed: Long, config: Config) extends GameInfo {
  var state = new GameState(input, seed)
  val pieces = state.source

  var path = ""
  while (!state.gameOver) {
    val (next, chunk) = Paths.findBest(state, config.depth.get)
    state = next
    val section = chunk.map(_.toString).mkString("")
    path += section
    if (config.debug.contains('v')) System.out.println(s"${state.board}$section\nscore: ${state.score}\n --------")
  }

  val output = Output(input.id, seed, config.tag, path)
  val moveScore = state.score
}

case class GameTile(input: Input, seed: Long, config: Config) extends GameInfo {
  var state = new GameState(input, seed)
  val pieces = state.source

  var path = ""
  while (!state.gameOver) {
    val pieces = Tiling.fill2(state, state.source.map(_.maxHeight).max)
    pieces.foreach { piece =>
      val chunk = try {
        Paths.findPowerPath(state.board, state.source.head.enter(state.board), piece, config).get
      } catch {
        case _: java.util.NoSuchElementException =>
          println(s"Borked placement for ${state.source.head.enter(state.board)} to $piece on ${state.board}")
          Paths.findBest(state, 1)._2.map(_.toString).mkString("")
      }
      try {
        state = state(Command(chunk))
        path += chunk
      } catch {
        case _: IllegalArgumentException =>
          println(s"Borked path for $chunk")
          val c2 = Paths.findPath(state.board, state.source.head.enter(state.board), piece).get.map(_.toString).mkString("")
          state = state(Command(c2))
          path += c2
      }
      if (config.debug.contains('v')) System.out.println(s"${state.board}$chunk\nscore: ${state.score}\n --------")
    }
  }

  val output = Output(input.id, seed, config.tag, path)
  val moveScore = state.score
}
