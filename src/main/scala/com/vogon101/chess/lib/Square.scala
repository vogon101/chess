package com.vogon101.chess.lib

/**
  * Square
  *
  * Created by fredd
  */
case  class Square(rank: Int, file: Int, piece: Option[Piece] = None) {

  def rankName: Int = rank + 1

  def fileName: String = List("a","b","c","d","e","f","g","h")(file)

  def name: String = fileName + rankName.toString

  def isEmpty: Boolean = piece.isEmpty

  override def toString: String = s"Square($rank, $file, $piece, $name)"

  def removePiece: Square = this.copy(piece = None)
  def addPiece(newPiece:Piece): Square = this.copy(piece = Some(newPiece))


}
