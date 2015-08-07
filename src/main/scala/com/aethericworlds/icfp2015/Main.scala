package com.aethericworlds.icfp2015

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
}

case class Config(files: List[String] = Nil, timeLimit: Option[Int] = None, memoryLimit: Option[Int] = None, phrases: List[String] = Nil)
