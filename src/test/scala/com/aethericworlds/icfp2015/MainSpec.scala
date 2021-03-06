package com.aethericworlds.icfp2015

import org.json4s._
import org.scalatest._

class MainSpec extends FunSpec with Matchers with Inspectors with AppendedClues {
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
        Main.parseArgs(Array("-p", "Ei!")) should equal (Config(Nil, None, None, List("Ei!")))
        Main.parseArgs(Array("-p", "Ei!", "-p", "Elspeth!")) should equal (Config(Nil, None, None, List("Ei!", "Elspeth!")))
      }
    }

    describe ("loadInputs") {
      it("should be able to load problem_0.json") {
        val input = Main.loadInputs(List("src/test/resources/problem_0.json"))(0)
        input.id should equal (JInt(0))
        input.sourceLength should equal (100)
        input.units should have length (18)
        input.units(1) should equal (Piece(Set(Cell(0, 0), Cell(2, 0)), Cell(1, 0)))
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

  describe ("Board") {
    describe ("Ordered") {
      it ("should prefer boards with stuff at the bottom") {
        Board(2, 2, Set(Cell(0, 1))) should be < (Board(2, 2, Set(Cell(0, 0))))
        Board(2, 2, Set(Cell(1, 1))) should be < (Board(2, 2, Set(Cell(0, 0))))
        Board(2, 2, Set(Cell(0, 1))) should be < (Board(2, 2, Set(Cell(1, 0))))
        Board(2, 2, Set(Cell(1, 1))) should be < (Board(2, 2, Set(Cell(1, 0))))
      }
      it ("should prefer boards with stuff at the left if heights are equal") {
        Board(2, 2, Set(Cell(0, 0))) should be < (Board(2, 2, Set(Cell(1, 0))))
        Board(2, 2, Set(Cell(0, 1))) should be < (Board(2, 2, Set(Cell(1, 1))))
      }
      it ("should maintain preferences even with other crap on the board") {
        Board(2, 2, Set(Cell(0, 1),Cell(1, 1))) should be < (Board(2, 2, Set(Cell(0, 0),Cell(0, 1))))
        Board(2, 2, Set(Cell(0, 1),Cell(1, 1))) should be < (Board(2, 2, Set(Cell(0, 0),Cell(1, 1))))
        Board(2, 2, Set(Cell(0, 1),Cell(1, 1))) should be < (Board(2, 2, Set(Cell(1, 0),Cell(0, 1))))
        Board(2, 2, Set(Cell(0, 1),Cell(1, 1))) should be < (Board(2, 2, Set(Cell(1, 0),Cell(1, 1))))
      }
    }
  }

  describe ("Piece") {
    val p = Piece(Set(Cell(0, 0), Cell(2, 0)), Cell(1, 0))

    it ("should properly reflect rotational congruency") {
      p.cw should not (equal (p))
      p.cw.cw should not (equal (p))
      p.cw.cw.cw should equal (p)

      p.cw.cw.cw.hashCode should equal (p.hashCode)
    }

    it ("should properly reflect translational congruency") {
      p(CommandW) should not (equal (p))
      p(CommandW)(CommandE) should equal (p)

      p(CommandW)(CommandE).hashCode should equal (p.hashCode)
    }
  }

  describe ("Command translation") {
    it ("should translate Ei!") {
      Command("Ei!") should equal (List(CommandE, CommandSW, CommandW))
    }

    it ("should translate Ia! Ia!") {
      Command("Ia! Ia!") should equal (List(CommandSW, CommandSW, CommandW, CommandSE, CommandSW, CommandSW, CommandW))
    }

    it ("should translate Yuggoth") {
      Command("Yuggoth") should equal (List(CommandE, CommandCCW, CommandSW, CommandSW, CommandSE, CommandCCW, CommandSW))
    }

    it ("should translate R'lyeh") {
      Command("R'lyeh") should equal (List(CommandCW, CommandW, CommandSE, CommandE, CommandE, CommandSW))
    }

    it ("should translate worshipping chants") {
      Command("Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn") should equal (List(
        CommandW, CommandSW, CommandW, CommandSE, CommandSW, CommandSE, CommandCCW, CommandSW,
        CommandSE, CommandSE, CommandSW, CommandSE, CommandCCW, CommandW, CommandSE, CommandSW,
        CommandE, CommandSW, CommandSE, CommandE, CommandCCW, CommandSW, CommandCCW, CommandSE,
        CommandSW, CommandCCW, CommandSE, CommandCW, CommandW, CommandSE, CommandE, CommandE,
        CommandSW, CommandSE, CommandCCW, CommandSW, CommandSW, CommandSW, CommandW, CommandSE,
        CommandSW, CommandSW, CommandSE, CommandSE, CommandE, CommandSW, CommandCCW, CommandSW,
        CommandSW, CommandSE
      ))
    }
  }

  describe ("Pinning the Game") {
    it ("should have the proper piece sequence") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val game = Game(input, "", 0, Config())
      game.pieces.take(10).map(_.toString).mkString("\n") should equal (
"""P#
Cell(0,0)

P#
Cell(0,0)

P###
 ##[]
Cell(0,0)

P###
 ##[]
Cell(0,0)

##P###
Cell(1,0)

P###
 ##[]
Cell(0,0)

##P###
Cell(1,0)

P#
Cell(0,0)

P###
Cell(0,0)

##P###
Cell(1,0)
""")
    }

    it ("should have the proper destination sequence") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val game = Game(input, """
iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeee
mmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiii
piimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm
""", 0, Config())
      game.placements.take(4).map(_._1.board.toString).mkString("\n") should equal (
"""[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 ##[][][][][][][][][]

[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 ####[][][][][][][][]

[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][]####[][][][][][]
 ######[][][][][][][]

[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][][][][][][][][][]
 [][][][][][][][][][]
[][]########[][][][]
 ######[]##[][][][][]
""")
    }

    it ("should compute the proper score") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val game = Game(input, """
iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeee
mmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiii
piimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm
""", 0, Config())
      game.moveScore should equal (61)
    }

    it ("should serialize properly") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val game = Game(input, """
iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeee
mmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiii
piimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm
""", 0, Config())
      Main.formatOutputs(List(game.output)) should equal (
"""[{"problemId":6,"seed":0,"solution":"iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeeemmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiiipiimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm"}]""")
    }

    it ("should function properly with a command Stream") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val game = Game(input, Stream.continually("Ei! ").flatten, 0, Config(phrases = List("Ei!")))
      game.moveScore should equal (20)
      game.powerScore should equal (450)
      game.totalScore should equal (470)
      game.output.solution.length should equal (100)
    }

    it ("should compute the proper score for the long input") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val game = Game(input, """
iiiiiiiimmiiiiiimimmiiiimimimmimimimimmimimimeemimeeeemimim
imimiiiiiimmeemimimimimiimimimmeemimimimmeeeemimimimmiiiiii
pmiimimimeeemmimimmemimimimiiiiiimeeemimimimimeeemimimimmii
iimemimimmiiiipimeeemimimmiiiippmeeeeemimimimiiiimmimimeemi
mimeeeemimimiiiipmeeemmimmiimimmmimimeemimimimmeeemimiiiiip
miiiimmeeemimimiiiipmmiipmmimmiippimemimeeeemimmiipppmeeeee
mimimmiimipmeeeemimimiimmeeeeemimmeemimmeeeemimiiippmiippmi
iimmiimimmmmmeeeemimmiippimmimimeemimimimmeemimimimmeemimim
imiimimimeeemmimimmmiiiiipimeemimimimmiiiimimmiiiiiiiimiimi
mimimeeemmimimimmiiiiiimimmemimimimimmimimimeemimiiiiiiiimi
iiimimimiimimimmimmimimimimmeeeemimimimimmmimimimimeemimimi
mimmmemimimmiiiiiiimiimimimmiiiiiimeeeeemimimimimmimimimmmm
emimimmeeeemimimimmiimimimmiiiiiipmeeeeemimimimimmiiiiimmem
imimimimmmmimimmeeeemimimimimeeemimimimmiimimimeeemmimimmii
iiiiimimiiiiiimimmiiiiiiiimmimimimimiiiimimimeemimimimimmee
emimimimimiiiiiiimiiiimimmemimimimmeemimimimeeemmimimmiiiii
immiiiipmmiiimmmimimeemimimeeemmimmiiiippmiiiimiiippimiimim
eemimimeeeemimimiiiipmeemimimiimiimimmimeeemimimmippipmmiim
emimmipimeeeemimmeemimiippimeeeeemimimmmimmmeeeemimimiiipim
miipmemimmeeeemimimiipipimmipppimeeemimmpppmmpmeeeeemimmemm
""", 0, Config())
      game.moveScore should equal (3261)
    }

    it ("should complain if there is too much input") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      val ex = the[IllegalArgumentException] thrownBy { Game(input, """
iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeee
mmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiii
piimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm
eee
""", 0, Config()) }
      ex.getMessage should include ("didn't consume all")
    }

    it ("should complain if you rotate to a congruent position") {
      val input = Main.loadInputs(List("src/test/resources/problem_7.json"))(0)
      val ex = the[IllegalArgumentException] thrownBy { Game(input, """a111""", 0, Config()) }
      ex.getMessage should include ("Repeated position in path")
    }

    it ("should complain if you wiggle back to a congruent position") {
      val input = Main.loadInputs(List("src/test/resources/problem_7.json"))(0)
      val ex = the[IllegalArgumentException] thrownBy { Game(input, """ep""", 0, Config()) }
      ex.getMessage should include ("Repeated position in path")
    }

    it ("shouldn't complain on a generated sequence") {
      val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
      noException shouldBe thrownBy { Game(input, """
blllllllllblllllllllballllllldddlbaaaallllklbaaaaaalllbaaaaaaaapaaaaallllpbllll
lllllbllllllllklpalllllllkplllllllllpaaallllllbaaaaallllbaaaaaaaaddlpbllllllldl
paaallllllbaaaaallllpaaaaaaaldplllllllldddlbaaallllllbaaaaallldddlpaaaaaaaddlpa
alllllllbblllllllllpaaallllldddlbaaaaallldddlpaaaaaaaddlplllllllllbaalllllllbaa
aaalllddlbaaaaaallklpaaaaaaakapbllllllldlballllllllballllllddlpaaaallllklpaaaaa
aakapblllllllklpaaaalllldpallllllddlpaaaaaaaddlpaaaaaallddlpllllllldlpaaalllllp
aaaaaaaalpaaaaaalllbblllllllkapaaaaaalakpblllllllllpaaallllllbaaaaaallklpaallll
llbaaaalllddlpblllllllkklpaaaaaaaalpaaaaaalklpaaaaaalakpallllllllballllllddlpaa
aaaalllpaaaaaaaalpblllllllllbaaaallllddlbaaaaaallddlpllllllllkklpaaaaaalakpalll
lllllpaaallllllpaaaaaallklplllllllddlbaallllllbaaaalllpaaaaaalklppaaaaaakapllll
llldbaallllllbaaalllllpaaaaaalakplllllllllpaaallllllbaaaaalllddlbaaaaaallklplll
lllllbblllllllbaaaaaaaalpaaaallllkklpaaaaaaalpaalllllklklpaaaaaaaalpaaaalllddlp
aaaaaaaalpbllllllldlbaalllllllpaaaaaaalddlbaaaaaaaddlplllllllldddlbaaalllllddlb
aaaalllklpaaaaaalllpaaaaaaaddlpallllllddldlpaaaaalllddlbaaaaaallpaaaaaaaddlpbll
llllldlpaalllllllpaaaallllklpaaaaaalakpaaallllddldlbbaaaaalllddlbaallllldlpaaaa
aalklplllllllpaaaaaaklppaaaaaakaplllllllpaalllllklpaaaalllklpaaaaaaaapbllllllld
lpalllllllbaaaaaallklpaaalllldlpaaaaaaalklbaaaaaaaapaaalllllkapaallllllddlpaaaa
lllddlpllllllkkllpaalllllllbaaaaaaalklballlllllbaaaallllpaaaaaaaddlpaaaaallllpa
aaaaalakplllllllllbaallllllddlbaaaallllklbaaaalllklbaaaaaallklpaaaaaaaalpaallll
llddlpaaaaaallpaaaaaaaapllllllkkllpallllllllbaaallllllplllllllldddlbaaallllllb
""", 0, Config()) }
    }
  }

  describe ("Paths") {
    describe ("reachable") {
      it ("should generate a locking path for available positions") {
        val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
        val start = GameState(input.board, input.units)
        val paths = Paths(input.board, input.units(0))
        forAll (paths.reachable) { case (where, how) =>
          val end = start(how)
          end.piece should equal (None)
          end.board should equal (start.board + where)
        }
      }
    }
    describe ("findPath") {
      it ("should find a short path to the goal") {
        val input = Main.loadInputs(List("src/test/resources/problem_6.json"))(0)
        val start = GameState(input.board, input.units)
        val paths = Paths(input.board, input.units(0))
        forAll (paths.reachable) { case (where, how) =>
          val pathOpt = Paths.findPath(input.board, input.units(0).enter(input.board), where)
          pathOpt should not (equal (None))
          pathOpt.get.size should equal (how.size +- 10)
          // println("where: " + where + "path: " + pathOpt)
          // val end = pathOpt.get.foldLeft(start){ (s, c) => println(c + "\n" + s.piece.map(s.board + _).getOrElse(s.board)); s(c) }
          val end = start(pathOpt.get)
          end.piece should equal (None)
          end.board should equal (start.board + where)
        }
     
      }
    }
  }

  describe ("Tiling") {
    it("should be able to fill a simple case") {
      val input = Main.loadInputs(List("src/test/resources/problem_1.json"))(0)
      val state = new GameState(input, 0)
      val pieces = Tiling.fill2(state, state.source.map(_.maxHeight).max)
      pieces should have size (15)
      forAll (pieces) { p => p.maxY should equal (14) }
    }
  }

  describe ("PriorityQueue") {
    it("should prefer large numbers") {
      scala.collection.mutable.PriorityQueue(1, 2, 3).dequeue should equal (3)
    }
  }
}
