package com.vogon101.chess.lib.ai.minimax

import com.vogon101.chess.lib.Game
import com.vogon101.chess.lib.ai.ChessAI
import com.vogon101.chess.lib.core.{Board, Colour}

/**
  * MiniMaxAI
  *
  * Created by fredd
  */
class MiniMaxAI(c: Colour) extends ChessAI {

  override val colour: Colour = c

  def evaluate(b: Board): Int = {
    val s = if (b.isCheckMate(c)) -1000
    else if (b.isCheckMate(c.otherColour)) 1000
    else b.pieces(c).map(_._1.value).sum - b.pieces(c.otherColour).map(_._1.value).sum
    val mod = if (b.isCheck(c)) -2 else if (b.isCheck(c.otherColour)) 2 else 0
    s + mod
  }

  def score(board: Board, levels: Int, nextPlayer: Colour): Int = {

    val possibleMoves = board.pieces(nextPlayer).flatMap { case (piece, currentSquare) =>
      val targets = piece.possibleMoves(currentSquare, board)
      targets.map(t => (t, board.movePiece(currentSquare, t)))
        .filter(_._2._1)
        .filterNot {case (_, (_, b)) =>
          b.isCheck(nextPlayer)
        }
        .map { case (t, (_, b)) =>
          val b_score = if (levels == 0) evaluate(b) else score(b, levels - 1, nextPlayer.otherColour)
          b_score
        }
    }

    //println(possibleMoves)

    if (nextPlayer == colour) possibleMoves.max
    else possibleMoves.min

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

      println(s"Piece: $piece (${currentSquare.name}), Boards: ${boards.length}")

      boards.map {case (t, (_, board)) =>
        val b_score = score(board, 2, colour.otherColour)
        (b_score, currentSquare, t)
      }
    }
    val move = possibleMoves.maxBy(_._1)

    println(possibleMoves.sortBy(_._1))
    println(move)
    println(possibleMoves.map(_._1))

    game.nextMove(move._2, move._3)

  }
}

