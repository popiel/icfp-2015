package com.aethericworlds.icfp2015

import org.scalatest._

class MainSpec extends FunSpec with Matchers with AppendedClues {
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


  describe("Cell") {
    it ("should move in the cardinal directions cleanly") {
      withClue("e") { Cell(0, 0).e should equal (Cell(1, 0)) }
      withClue("w") { Cell(0, 0).w should equal (Cell(-1, 0)) }
      withClue("se, even") { Cell(0, 0).se should equal (Cell(0, 1)) }
      withClue("sw, even") { Cell(0, 0).sw should equal (Cell(-1, 1)) }
      withClue("ne, even") { Cell(0, 0).ne should equal (Cell(0, -1)) }
      withClue("nw, even") { Cell(0, 0).nw should equal (Cell(-1, -1)) }

      withClue("se, odd") { Cell(0, 1).se should equal (Cell(1, 2)) }
      withClue("sw, odd") { Cell(0, 1).sw should equal (Cell(0, 2)) }
      withClue("ne, odd") { Cell(0, 1).ne should equal (Cell(1, 0)) }
      withClue("nw, odd") { Cell(0, 1).nw should equal (Cell(0, 0)) }
    }

    it ("should rotate clockwise properly") {
      Cell( 1,  0).cw should equal (Cell( 0,  1))
      Cell( 0,  1).cw should equal (Cell(-1,  1))
      Cell(-1,  1).cw should equal (Cell(-1,  0))
      Cell(-1,  0).cw should equal (Cell(-1, -1))
      Cell(-1, -1).cw should equal (Cell( 0, -1))
      Cell( 0, -1).cw should equal (Cell( 1,  0))

      Cell( 2,  0).cw should equal (Cell( 1,  2))
      Cell( 1,  1).cw should equal (Cell( 0,  2))
      Cell( 1,  2).cw should equal (Cell(-1,  2))
    }

    it ("should rotate counterclockwise properly") {
      Cell( 0,  1).ccw should equal (Cell( 1,  0))
      Cell(-1,  1).ccw should equal (Cell( 0,  1))
      Cell(-1,  0).ccw should equal (Cell(-1,  1))
      Cell(-1, -1).ccw should equal (Cell(-1,  0))
      Cell( 0, -1).ccw should equal (Cell(-1, -1))
      Cell( 1,  0).ccw should equal (Cell( 0, -1))

      Cell( 1,  2).ccw should equal (Cell( 2,  0))
      Cell( 0,  2).ccw should equal (Cell( 1,  1))
      Cell(-1,  2).ccw should equal (Cell( 1,  2))
    }

    it ("should negate correctly") {
      -Cell(0, 1) should equal (Cell(-1, -1))
      -Cell(-1, 1) should equal (Cell(0, -1))
      -Cell(1, 0) should equal (Cell(-1, 0))
      -Cell(-1, 0) should equal (Cell(1, 0))
      -Cell(0, -1) should equal (Cell(-1, 1))
      -Cell(-1, -1) should equal (Cell(0, 1))
      -Cell(0, 2) should equal (Cell(0, -2))
      -Cell(0, 2) should equal (Cell(0, -2))
    }

    it ("should add correctly") {
      Cell(0, 1) + Cell(0, 1) should equal (Cell(1, 2)) withClue("a")
      Cell(0, 1) + Cell(0, -1) should equal (Cell(1, 0)) withClue("b")
      Cell(0, 1) + Cell(-1, 1) should equal (Cell(0, 2)) withClue("c")
      Cell(0, 1) + Cell(-1, -1) should equal (Cell(0, 0)) withClue("d")
      Cell(0, 1) + Cell(1, 0) should equal (Cell(1, 1)) withClue("e")

      Cell(1, 0) + Cell(0, 1) should equal (Cell(1, 1)) withClue("f")
      Cell(1, 0) + Cell(0, -1) should equal (Cell(1, -1)) withClue("g")
      Cell(1, 0) + Cell(-1, 1) should equal (Cell(0, 1)) withClue("h")
      Cell(1, 0) + Cell(-1, -1) should equal (Cell(0, -1)) withClue("i")
      Cell(1, 0) + Cell(1, 0) should equal (Cell(2, 0)) withClue("j")
    }

    it ("should subtract correctly") {
      Cell(0, 1) - Cell(0, 1) should equal (Cell(0, 0)) withClue("a")
      Cell(0, 1) - Cell(0, -1) should equal (Cell(0, 2)) withClue("b")
      Cell(0, 1) - Cell(-1, 1) should equal (Cell(1, 0)) withClue("c")
      Cell(0, 1) - Cell(-1, -1) should equal (Cell(1, 2)) withClue("d")
      Cell(0, 1) - Cell(1, 0) should equal (Cell(-1, 1)) withClue("e")

      Cell(1, 0) - Cell(0, 1) should equal (Cell(0, -1)) withClue("f")
      Cell(1, 0) - Cell(0, -1) should equal (Cell(0, 1)) withClue("g")
      Cell(1, 0) - Cell(-1, 1) should equal (Cell(1, -1)) withClue("h")
      Cell(1, 0) - Cell(-1, -1) should equal (Cell(1, 1)) withClue("i")
      Cell(1, 0) - Cell(1, 0) should equal (Cell(0, 0)) withClue("j")
    }
  }

  describe ("Source") {
    it("should produce the documented sequence") {
      new Source(17).take(10).toList should equal (List(0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873, 16117))
    }
  }
}
