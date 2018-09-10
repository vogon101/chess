package com.vogon101.chess.lib.core

/**
  * Square
  *
  * Created by fredd
  */
case  class Square(rank: Int, file: Int, piece: Option[Piece] = None) {

  lazy val rankName: Int = rank + 1

  lazy val fileName: String = List("a","b","c","d","e","f","g","h").reverse(file)

  lazy val name: String = fileName + rankName.toString

  lazy val isEmpty: Boolean = piece.isEmpty

  def toRepresentation(colour: Colour): Array[Double] = {
    val s: Double = piece match {
      case None => 0
      case Some(x: Piece) if x.colour == colour => 1
      case _ => -1
    }
    val p: List[Double] = piece match {
      case Some(x: King)   => List(s , 0d, 0d, 0d)
      case Some(x: Queen)  => List(0d, s , s , 0d)
      case Some(x: Rook)   => List(0d, s , 0d, 0d)
      case Some(x: Bishop) => List(0d, 0d, s , 0d)
      case Some(x: Knight) => List(0d, 0d, 0d, s )
      case Some(x: Pawn)   => List(s , s , s , s )
      case _               => List(0d, 0d, 0d, 0d)
    }
    p.toArray
  }

  override val toString: String = s"Square($rank, $file, $piece, $name)"

  def removePiece: Square = this.copy(piece = None)
  def addPiece(newPiece:Piece): Square = this.copy(piece = Some(newPiece))


}
