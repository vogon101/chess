package com.vogon101.chess.lib

/**
  * Game
  *
  * Created by fredd
  */
class Game {

  //This bitch be mutable af
  private var _board = Board.startingBoard
  def board: Board = _board

  private var _nextColour: Colour = White
  def nextColour: Colour = _nextColour


  def nextMove(start: Square, end: Square): (Boolean, Boolean) = {
    println(nextColour)
    if (start.piece.isDefined && start.piece.get.colour == nextColour) {
      println ("Right Colour")

      val (success, b) = board.movePiece(start, end)

      if (success) {

        println ("Success")
        if (b.isCheck(nextColour)) {
          println("Check Fail")
          (false, false)
        }
        else {
          println("Real Success")
          _board = b
          _nextColour = nextColour.otherColour
          (true, b.isCheckMate(nextColour))
        }
      }
      else (false, false)

    } else (false, false)
  }

  def nextMove(start:String, end:String): (Boolean, Boolean) = nextMove(board.getSquare(start), board.getSquare(end))

}
