package com.vogon101.chess.lib.ai.minimax

import com.vogon101.chess.lib.Game
import com.vogon101.chess.lib.ai.ChessAI
import com.vogon101.chess.lib.ai.networks.NeuralNetwork
import com.vogon101.chess.lib.core.{Board, Colour}

import scala.math.{max, min}

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * NNAlphaBetaAI
  *
  * Created by fredd
  */
class NNAlphaBetaAI(c: Colour, val network: NeuralNetwork, difficulty: Int = 2) extends ChessAI {

  override val colour: Colour = c
  private val INF = 10000000d
  private val centreSquares = List("c4", "d4", "e4", "f4", "c5", "d5", "e5", "f5")

  def evaluate(b: Board, lineage: List[Board]): Double = {

    val repr = b.squares.flatMap(_.toRepresentation(c)).toArray

    network.tick(DenseVector(repr ++ Array.fill(network.output.length - repr.length)(0d)))

    network.output(network.output.length - 1)

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

