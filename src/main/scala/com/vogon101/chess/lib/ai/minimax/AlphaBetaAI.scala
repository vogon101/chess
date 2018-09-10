package com.vogon101.chess.lib.ai.minimax

import com.vogon101.chess.lib.Game
import com.vogon101.chess.lib.ai.ChessAI
import com.vogon101.chess.lib.core.{Board, Colour, Piece, Square}

import math.{max, min}
import scala.annotation.tailrec

/**
  * AlphaBetaAI
  *
  * Created by fredd
  */
class AlphaBetaAI(c: Colour, val difficulty: Int = 2) extends ChessAI {

  override val colour: Colour = c
  private val INF = 10000000d
  private val centreSquares = List("c4", "d4", "e4", "f4", "c5", "d5", "e5", "f5")

  def evaluate(b: Board, lineage: List[Board]): Double = {

    val myCentreSquares = centreSquares.map(b.getSquare)

    val s = if (b.isCheckMate(c)) -1000
      else if (b.isCheckMate(c.otherColour)) 1000
      else
      b.pieces(c).map{p =>
        p._1.value + myCentreSquares.map(
          cs => p._1.can_attack(p._2, b)(cs)
        ).count(x => x) * 0.15
      }.sum - b.pieces(c.otherColour).map{ p =>
        p._1.value + myCentreSquares.map(
          cs => p._1.can_attack(p._2, b)(cs)
        ).count(x => x) * 0.1
      }.sum
    val mod = if (b.isCheck(c)) -2.5 else if (b.isCheck(c.otherColour)) 2.5 else 0

    s +
      mod +
      lineage.zipWithIndex.map{
        case (l_b, i) => (0.2 / (i + 1)) * evaluate(l_b, List())
      }.sum
  }

  def score(board: Board, levels: Int, nextPlayer: Colour, alpha_orrig: Double, beta_orrig: Double, parents: List[Board]): Double = {

    var alpha = alpha_orrig
    var beta = beta_orrig

    if (levels == 0) evaluate(board, parents)
    else {

      val new_lineage: List[Board] = board :: parents

      val children = board.pieces(nextPlayer).flatMap {
        case (piece, currentSquare) =>
          piece.possibleMoves(currentSquare, board).map(t => (piece, currentSquare, t))
      }

      //Maximising Colour
      if (nextPlayer == colour) {

        var value = -INF
        var noBreak = true
        var i = 0

        while(noBreak && i < children.length) {
          val child = children(i)
          val (r, b) = board.movePiece(child._2, child._3)
          if (r && !b.isCheck(nextPlayer)){
            value = max(value, score(b, levels - 1, nextPlayer.otherColour, alpha, beta, new_lineage))
            alpha = max(alpha, value)
            if (alpha >= beta) noBreak = false
          }
          i += 1
        }
        value

      }
      else {

        var value = INF
        var noBreak = true
        var i = 0

        while(noBreak && i < children.length) {
          val child = children(i)
          val (r, b) = board.movePiece(child._2, child._3)
          if (r && !b.isCheck(nextPlayer)){
            value = min(value, score(b, levels - 1, nextPlayer.otherColour, alpha, beta, new_lineage))
            beta = min(beta, value)
            if (alpha >= beta) noBreak = false
          }
          i += 1
        }
        value

      }
    }
  }

  override def move(game: Game): (Boolean, Boolean) = {

    val possibleMoves = game.board.pieces(colour).flatMap { case (piece, currentSquare) =>
      val targets = piece.possibleMoves(currentSquare, game.board)
      val boards = targets
        .map(t => (t, game.board.movePiece(currentSquare, t)))
        .filter(_._2._1)
        .filterNot {case (_, (_, b)) =>
          b.isCheck(colour)
        }

      //println(s"Piece: $piece (${currentSquare.name}), Boards: ${boards.length}")

      boards.map {case (t, (_, board)) =>
        val b_score = score(board, difficulty, colour.otherColour, -INF, INF, List(board))
        (b_score, currentSquare, t)
      }
    }
    val move = possibleMoves.maxBy(_._1)

    //println(possibleMoves.sortBy(_._1))
    //println(move)
    //println(possibleMoves.map(_._1))

    game.nextMove(move._2, move._3)

  }
}


