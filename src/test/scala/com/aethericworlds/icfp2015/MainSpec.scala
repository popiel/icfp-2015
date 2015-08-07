package com.aethericworlds.icfp2015

import org.scalatest._

class MainSpec extends FunSpec with Matchers {
  describe ("Coordinator") {
    describe ("parseArgs") {
      it ("should read filenames properly") {
        Main.parseArgs(Array("-f", "gleep.json")) should equal (Config(List("gleep.json"), None, None, Nil))
        Main.parseArgs(Array("-f", "gleep.json", "-f", "beep.json")) should equal (Config(List("gleep.json", "beep.json"), None, None, Nil))
      }
      it ("should read time limits properly") {
        Main.parseArgs(Array("-t", "10")) should equal (Config(Nil, Some(10), None, Nil))
        Main.parseArgs(Array("-t", "10", "-t", "5")) should equal (Config(Nil, Some(5), None, Nil))
      }
      it ("should read memory limits properly") {
        Main.parseArgs(Array("-m", "10")) should equal (Config(Nil, None, Some(10), Nil))
        Main.parseArgs(Array("-m", "10", "-m", "5")) should equal (Config(Nil, None, Some(5), Nil))
      }
      it ("should read phrases properly") {
        Main.parseArgs(Array("-p", "Ei!")) should equal (Config(Nil, None, None, List("ei!")))
        Main.parseArgs(Array("-p", "Ei!", "-p", "Elspeth!")) should equal (Config(Nil, None, None, List("ei!", "elspeth!")))
      }
    }
  }
}
