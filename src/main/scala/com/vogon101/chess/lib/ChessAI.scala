package com.vogon101.chess.lib

/**
  * ChessAI
  *
  * Created by fredd
  */
abstract class ChessAI {

  val colour: Colour

  def move(game: Game): (Boolean, Boolean)

}

class MiniMaxAI(c: Colour) extends ChessAI {

  override val colour: Colour = c

  def evaluate(b: Board): Int = {
    if (b.isCheckMate(c)) -1000
    else if (b.isCheckMate(c.otherColour)) 1000
    else b.pieces.filter(_._1.colour == c).map(_._1.value).sum - b.pieces.filter(_._1.colour != c).map(_._1.value).sum
  }

  def score(board: Board, levels: Int, nextPlayer: Colour): Int = {

    val possibleMoves = board.pieces(nextPlayer).flatMap { case (piece, currentSquare) =>
      val targets = piece.possibleMoves(currentSquare, board)
      targets.map(t => (t, board.movePiece(currentSquare, t)))
        .filter(_._2._1)
        .map { case (t, (_, board)) =>
          val b_score = if (levels == 0) evaluate(board) else score(board, levels - 1, nextPlayer.otherColour)
          b_score
        }
    }

    if (nextPlayer == colour) possibleMoves.max
    else possibleMoves.min

  }

  override def move(game: Game): (Boolean, Boolean) = {

    val possibleMoves = game.board.pieces(colour).flatMap { case (piece, currentSquare) =>
          val targets = piece.possibleMoves(currentSquare, game.board)
          val boards = targets
            .map(t => (t, game.board.movePiece(currentSquare, t)))
            .filter(_._2._1)

          println(s"Piece: $piece (${currentSquare.name}), Boards: ${boards.length}")

          boards.map {case (t, (_, board)) =>
                val b_score = score(board, 1, colour.otherColour)
              (b_score, currentSquare, t)
            }
    }
    val move = possibleMoves.sortBy(_._1).head

    game.nextMove(move._2, move._3)

  }
}
