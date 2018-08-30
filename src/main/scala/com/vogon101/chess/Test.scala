package com.vogon101.chess

import com.vogon101.chess.lib._


/**
  * Test
  *
  * Created by fredd
  */
object Test extends App {

  var b = Board.startingBoard

  println(b.display())
  /*
  val game = List(("e2", "e4"), ("d7", "d5"), ("d5", "e4"), ("c8", "g4"))

  for ((s,e) <- game) {
    println(b.translateName(s))
    println(b.translateName(e))
    val res = b.movePiece(b.getSquare(s), b.getSquare(e))
    println(res._1)
    b = res._2
    println(b.display())
    println(b.display(Some(White)))
  }

  while (true) {
    val s = readLine("start > ")
    val e = readLine("end   > ")
    val res = b.movePiece(b.getSquare(s), b.getSquare(e))
    println(res._1)
    b = res._2
    println(b.display())
    println(b.display(Some(White)))
  }
  */

  /*
  val game = new Game()
  var running = true
  while (running) {
    val s = readLine("start > ")
    val e = readLine("end   > ")
    val res = game.nextMove(s, e)
    println(res)
    println(game.board.display())
    println("\n")
    running = !res._2
  }*/

  val game = new Game()
  val AI = new MiniMaxAI(Black)
  var running = true
  while (running) {

    println(game.board.getSquare("d1").piece.get.possibleMoves(game.board.getSquare("d1"), game.board))

    val s = readLine("start > ")
    val e = readLine("end   > ")
    val res1 = game.nextMove(s, e)
    println(res1)
    println(game.board.display())
    println("\n")
    running = !res1._2



    val res2 = AI.move(game)
    println(res2)
    println(game.board.display())
    println("\n")
    running = !res2._2

  }


}
