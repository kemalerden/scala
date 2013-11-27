package streams

import common._
import Bloxorz._

object Main extends App {

  object Level extends Solver with StringParserTerrain {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    val t = newNeighborsOnly(
      Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

      Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)),  Block(Pos(2, 1), Pos(3, 1))))

    val b = t == Set(
      (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream
  }
  println(Level.t)
  println(Level.b)

}
