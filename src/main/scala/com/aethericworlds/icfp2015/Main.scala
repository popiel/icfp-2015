package com.aethericworlds.icfp2015

import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}

object Main extends Coordinator {
  def main(args: Array[String]) {
    implicit val formats = Serialization.formats(NoTypeHints)

    val config = parseArgs(args)
    val outputs = for {
      input <- loadInputs(config.files)
      _ = if (config.debug contains 'b') System.err.println(s"Problem ${write(input.id)} board:\n${input.board}")
      _ = if (config.debug contains 'u') System.err.println(s"Problem ${write(input.id)} units:\n${input.units.map(_.toString).mkString("\n")}")
      seed <- config.seed.map(List(_)).getOrElse(input.sourceSeeds)
      game = if (config.depth == None) {
        val phrase = config.phrases.mkString("")
        Game(input, Stream.continually(phrase).flatten, seed, config)
      } else {
        GameSearch(input, seed, config)
      }
      _ = if (config.debug contains 'q') System.err.println(s"Problem ${write(input.id)}, seed $seed, pieces:\n${game.pieces.map(_.toString).mkString("\n")}")
      _ = if (config.debug contains 'h') System.err.println(s"Problem ${write(input.id)}, seed $seed, pieces:\n${game.pieces.map(_.maxHeight).mkString(" ")}")
      _ = if (config.debug contains 'r') System.err.println(s"Problem ${write(input.id)}, seed $seed, pieces:\n${game.pieces.map(_.maxRows).mkString(" ")}")
      _ = if (config.debug contains 's') System.err.println(s"Problem ${write(input.id)}, seed $seed, score ${game.totalScore}")
    } yield game.output
    println(formatOutputs(outputs))
  }
}

class Coordinator {
  def parseArgs(args: Array[String]): Config = {
    args.grouped(2).foldLeft(Config()) { (c, l) => (l(0), l(1)) match {
      case ("-f", file)   => c.copy(files = c.files :+ file)
      case ("-t", num)    => c.copy(timeLimit = Some(num.toInt))
      case ("-m", num)    => c.copy(memoryLimit = Some(num.toInt))
      case ("-p", phrase) => c.copy(phrases = c.phrases :+ phrase)
      case ("-c", num)    => c.copy(cores = Some(num.toInt))
      case ("-tag", tag)  => c.copy(tag = Some(tag))
      case ("-s", num)    => c.copy(seed = Some(num.toLong))
      case ("-d", flags)  => c.copy(debug = flags)
      case ("-depth", num)=> c.copy(depth = Some(num.toInt))
      case (opt, value)   => throw new IllegalArgumentException(s"Unrecognized option '$opt'")
    } }
  }

  implicit val formats = Serialization.formats(NoTypeHints)

  def loadInputs(files: List[String]): List[Input] =
    files.map { file => read[Input](io.Source.fromFile(file).getLines.mkString("\n")) }

  def formatOutputs(stuff: List[Output]) = write(stuff)
}

case class Config(
  files: List[String] = Nil,
  timeLimit: Option[Int] = None,
  memoryLimit: Option[Int] = None,
  phrases: List[String] = Nil,
  cores: Option[Int] = None,
  tag: Option[String] = None,
  seed: Option[Long] = None,
  debug: String = "",
  depth: Option[Int] = None
)

case class Input(id: JValue, units: List[Piece], width: Int, height: Int, filled: List[Cell], sourceLength: Int, sourceSeeds: List[Long]) {
  val board = Board(width = width, height = height, filled = filled.toSet)
}

case class Output(problemId: JValue, seed: Long, tag: Option[String], solution: String)

class Source(var seed: Long) extends Iterator[Int] {
  def next = {
    val output = (seed >> 16) & 0x7fff
    seed = (seed * 1103515245 + 12345) & 0xffffffff
    output.toInt
  }
  def hasNext = true
}
