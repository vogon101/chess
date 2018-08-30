package com.vogon101.chess.lib

import scala.math.abs

/**
  * Piece
  *
  * Created by fredd
  */
abstract class Piece(
             val name: String,
             val letter: String,
             val colour: Colour,
             val value: Int,
             val hasMoved: Boolean = false
           ) {

  def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean

  def possibleMoves(currentSquare: Square, board: Board): List[Square] =
    board.squares.filter(canMove(currentSquare, board)(_))

  def can_attack(currentSquare: Square, board: Board)(targetSquare: Square): Boolean =
    canMove(currentSquare, board)(targetSquare)

  override def toString: String = letter + colour.letter

  def moved: Piece

}

class King(c: Colour, hasM: Boolean = false)
  extends Piece("King", "K", c, 0, hasM) {

  override def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    if (!targetSquare.isEmpty) {
      targetSquare.piece match {
        case Some(k: King) => false
        case Some(p: Piece) if p.colour == colour => false
        case _ => true
      }
    }

    else {

      if (board.pieces.filter(_._1.colour != colour)
        .exists(X => X._1.can_attack(X._2, board)(targetSquare))
      ) false

      else if (
        abs(currentSquare.rank - targetSquare.rank) <= 1 &&
        abs(currentSquare.file - targetSquare.file) <= 1
      ) true

      else false

    }

  }

  override def can_attack(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    if (!targetSquare.isEmpty) {
      targetSquare.piece match {
        case Some(k: King) => false
        case Some(p: Piece) if p.colour == colour => false
        case _ => true
      }
    }

    else if (
      abs(currentSquare.rank - targetSquare.rank) <= 1 &&
        abs(currentSquare.file - targetSquare.file) <= 1
    ) true

    else false

  }

  override def moved: Piece = new King(colour, true)

}

class Rook(c: Colour, hasM: Boolean = false)
  extends Piece("Rook", "R", c, 5, hasM) {

  override def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    if (!targetSquare.isEmpty) {
      targetSquare.piece match {
        case Some(k: King) => false
        case Some(p: Piece) if p.colour == colour => false
        case _ => if (currentSquare.rank != targetSquare.rank && currentSquare.file != targetSquare.file) false
                  else board.pathClear(currentSquare, targetSquare)
      }
    }


  }

  override def moved: Piece = new Rook(colour, true)

}

class Bishop(c: Colour, hasM: Boolean = false)
  extends Piece("Bishop", "B", c, 3, hasM) {

  override def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    //TODO: returns true when space

    if (!targetSquare.isEmpty) {
      targetSquare.piece match {
        case Some(k: King) => false
        case Some(p: Piece) if p.colour == colour => false
        case _ => true
      }
    }

    else if (currentSquare.rank == targetSquare.rank || currentSquare.file == targetSquare.file) false

    else board.pathClear(currentSquare, targetSquare)

  }

  override def moved: Piece = new Bishop(colour, true)

}

class Queen(c: Colour, hasM: Boolean = false)
  extends Piece("Queen", "Q", c, 9, hasM) {

  override def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    if (!targetSquare.isEmpty) {
      targetSquare.piece match {
        case Some(k: King) => false
        case Some(p: Piece) if p.colour == colour => false
        case _ => true
      }
    }

    else board.pathClear(currentSquare, targetSquare)

  }

  override def moved: Piece = new Queen(colour, true)

}

class Knight(c: Colour, hasM: Boolean = false)
  extends Piece("Knight", "N", c, 3, hasM) {

  override def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    if (!targetSquare.isEmpty) {
      targetSquare.piece match {
        case Some(k: King) => false
        case Some(p: Piece) if p.colour == colour => false
        case _ =>
      }
    }

    else if (
      (abs(currentSquare.rank - targetSquare.rank) == 2
        && abs(currentSquare.file - targetSquare.file) == 1)
      || (abs(currentSquare.rank - targetSquare.rank) == 1
        && abs(currentSquare.file - targetSquare.file) == 2)
    ) true

    else false

  }

  override def moved: Piece = new Knight(colour, true)

}

class Pawn(c: Colour, hasM: Boolean = false)
  extends Piece("Pawn", "p", c, 1, hasM) {

  override def canMove(currentSquare: Square, board: Board)(targetSquare: Square): Boolean = {

    if (currentSquare.file == targetSquare.file) {

      if (!targetSquare.isEmpty) false
      else if (targetSquare.rank == currentSquare.rank + colour.direction) true
      else if (targetSquare.rank == currentSquare.rank + 2 * colour.direction  && !hasMoved)
        board.pathClear(currentSquare, targetSquare)
      else false

    }
    else if (abs(currentSquare.file - targetSquare.file) == 1) {
      if (!targetSquare.isEmpty)
        targetSquare.piece match {
          case Some(k: King) => false
          case Some(p: Piece) if p.colour == colour => false
          case _ => true
        }
      else if (targetSquare.rank == currentSquare.rank + colour.direction) true
      else false
    }
    else false

  }

  override def can_attack(currentSquare: Square, board: Board)(targetSquare: Square): Boolean =
    if (abs(currentSquare.file - targetSquare.file) == 1
        && targetSquare.rank == currentSquare.rank + colour.direction) true
    else false

  override def moved: Piece = new Pawn(colour, true)

}