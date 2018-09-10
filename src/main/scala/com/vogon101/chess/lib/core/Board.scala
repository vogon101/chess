package com.vogon101.chess.lib
import scala.math.{abs, min, max}

/**
  * Board
  *
  * Created by fredd
  */
class Board(start_board: Option[List[List[Square]]] = None, val kings: List[King] = List()) {

  //TODO: Logging

  val board: List[List[Square]] = start_board.getOrElse(
    Range(0, 8).map {rank =>
      Range(0, 8).map { file =>
        Square(rank, file, None)
      }.toList
    }.toList
  )

  lazy val squares: List[Square] = board.flatten

  def pieces(colour: Colour): List[(Piece, Square)] = squares.filter(X => !X.isEmpty && X.piece.get.colour == colour).map(X => (X.piece.get, X))

  lazy val pieces: List[(Piece, Square)] = squares.filterNot(_.isEmpty).map(X => (X.piece.get, X))

  def squareIsAttacked(square: Square, byColour: Colour):Boolean = {

    val piecies_list = squares.filter(X => X.piece.isDefined && X.piece.get.colour == byColour).map(X => (X, X.piece.get))

    piecies_list.exists {
      case (s,p) => p.can_attack(s, this)(square)
    }

  }

  def squareOf(piece: Piece): Option[Square] = squares.find(_.piece.contains(piece))

  def pathClear(start: Square, end: Square, log: (Any) => Unit = (x: Any) => {}): Boolean = {

    assert(start != end)

    if (start.rank != end.rank) {
      if (start.file != end.file) diagonalClear(start, end, log)
      else fileClear(start, end)
    }
    else rankClear(start,end)

  }

  private def diagonalClear(start: Square, end: Square, log: (Any) => Unit = (x: Any) => {}): Boolean = {
    log(s"Diagonal Clear $start -> $end")
    if (abs(start.rank - end.rank) != abs(start.file - end.file)) false
    else {
      //println("Diagonal")
      val lowerRank = min(start.rank, end.rank)
      val lowerFile = min(start.file, end.file)
      val upperRank = max(start.rank, end.rank)

      if ((lowerRank == start.rank) == (lowerFile == start.file)) {
        val s = Range(1, upperRank - lowerRank)
          .map(idx => board(lowerRank + idx)(lowerFile + idx))
        log("LTR")
        log(s)
        s.forall(_.isEmpty)
      } else {
        val s = Range(1, upperRank - lowerRank)
          .map(idx => board(upperRank - idx)(lowerFile + idx))
        log("RTL")
        log(s)
        s.forall(_.isEmpty)
      }
    }
  }

  private def fileClear(start: Square, end: Square): Boolean =
    if (start.rank < end.rank) {
      val s = board.slice(start.rank + 1, end.rank).map(_(start.file))
      s.forall(_.isEmpty)
    } else {
      val s = board.slice(end.rank + 1, start.rank).map(_ (start.file))
      s.forall(_.isEmpty)
    }

  private def rankClear(start: Square, end: Square): Boolean =
    if (start.file < end.file) {
      val s = board(start.rank).slice(start.file + 1, end.file)
      s.forall(_.isEmpty)
    } else {
      val s = board(start.rank).slice(end.file + 1, start.file)
      s.forall(_.isEmpty)
    }

  //TODO:
  def display(showAttack:Option[Colour] = None): String = {
    board.zipWithIndex.map { case (rank, idx) =>
      (idx + 1).toString+ "|" + rank.map {
        case Square(_, _, Some(p)) => p.letter + p.colour.letter
        case x => if (showAttack.isDefined && squareIsAttacked(x, showAttack.get)) "x" + showAttack.get.letter
          else "  "
      }.mkString("|")+"|"
    }.mkString("\n") + "\n |h |g |f |e |d |c |b |a |"
  }

  def getSquare(name: String): Square = {

    val (number, letter) = translateName(name)

    board(number)(letter)

  }

  def getSquare(rank:Int, file: Int):Square = board(rank)(file)

  def translateName(name: String): (Int, Int) = {
    assert(name.length == 2)

    val letter = Board.LETTERS.indexOf(name.charAt(0).toString)
    val number = name.charAt(1).toString.toInt - 1

    (number, letter)

  }

  def setPiece(p: Option[Piece], rank: Int, file: Int):Board = {

    val sq = board(rank)(file).copy(piece = p)

    val r = board(rank).patch(file, List(sq), 1)

    p match {
      case Some(x: King) => new Board(Some(board.patch(rank, List(r), 1)), kings.filterNot(_.colour == x.colour) ++ List(x))
      case _ => new Board(Some(board.patch(rank, List(r), 1)), kings)
    }



  }

  def setPiece(p: Option[Piece], square: Square): Board = setPiece(p, square.rank, square.file)

  //TODO: Promotion
  //TODO: Castling
  def movePiece(start:Square, end:Square, log: (Any) => Unit = (x: Any) => {}): (Boolean, Board) = {

    log(start)
    log(end)

    //TODO: Check logic

    if (start.isEmpty) {
      log("Start empty")
      log(start)
      (false, this)
    }
    else if (start.piece.get.canMove(start, this)(end)) {
      (start.piece, end.piece) match {
        case (Some(k: King), Some(r: Rook)) if k.colour == r.colour =>
          log("Castling")
          if (start.file < end.file) {
            //Queenside
            val p_k = Some(k.moved)
            val p_r = Some(r.moved)
            val b1 = setPiece(None, start).setPiece(None, end)
            val b2 = b1.setPiece(p_k, b1.getSquare(start.rank , 5))
            val b3 = b2.setPiece(p_r, b2.getSquare(start.rank, 4))
            (true, b3)
          } else {
            //Kingside
            val p_k = Some(k.moved)
            val p_r = Some(r.moved)
            val b1 = setPiece(None, start).setPiece(None, end)
            val b2 = b1.setPiece(p_k, b1.getSquare(start.rank , 1))
            val b3 = b2.setPiece(p_r, b2.getSquare(start.rank, 2))
            (true, b3)
          }
        case (Some(ps: Piece), _) =>
          log("Can move")
          val p = Some(ps.moved)
          val b1 = setPiece(None, start)
          val b2 = b1.setPiece(p, end)
          (true, b2)

      }


    }
    else (false, this)

  }

  def isCheckMate(colour: Colour): Boolean = {
    if (isCheck(colour)) {
      val possibleMoves = pieces.filter(_._1.colour == colour).flatMap(X => X._1.possibleMoves(X._2, this).map(Y => (X._1, X._2, Y)))
      possibleMoves.forall(X => this.movePiece(X._2, X._3)._2.isCheck(colour))
    } else false
  }

  def isCheck(colour: Colour): Boolean = {
    val king = kings.find(_.colour == colour).get
    val kingSquare = squareOf(king).get
    squareIsAttacked(kingSquare, colour.otherColour)
  }

}

object Board {

  val LETTERS = List("a","b","c","d","e","f","g","h").reverse

  def startingBoard: Board = {

    val Kw = new King(White)
    val Kb = new King(Black)

    val Qw = new Queen(White)
    val Qb = new Queen(Black)

    val Rw1 = new Rook(White)
    val Rw2 = new Rook(White)
    val Rb1 = new Rook(Black)
    val Rb2 = new Rook(Black)

    val Nw1 = new Knight(White)
    val Nw2 = new Knight(White)
    val Nb1 = new Knight(Black)
    val Nb2 = new Knight(Black)

    val Bw1 = new Bishop(White)
    val Bw2 = new Bishop(White)
    val Bb1 = new Bishop(Black)
    val Bb2 = new Bishop(Black)

    val backRankWhite = List(Rw1, Nw1, Bw1, Kw, Qw, Bw2, Nw2, Rw2)
    val backRankBlack = List(Rb1, Nb1, Bb1, Kb, Qb, Bb2, Nb2, Rb2)

    val board = Range(0,8).map {
      case 0 => backRankWhite.zipWithIndex.map {
        case (p, i) => Square(0, i, Some(p))
      }
      case 7 => backRankBlack.zipWithIndex.map {
        case (p, i) => Square(7, i, Some(p))
      }
      case 1 => Range(0, 8).map {f =>
        Square(1, f, Some(new Pawn(White)))
      }.toList
      case 6 => Range(0, 8).map {f =>
        Square(6, f, Some(new Pawn(Black)))
      }.toList
      case r => Range(0,8).map(f => Square(r, f, None)).toList
    }.toList

    new Board(Some(board), List(Kw, Kb))

  }

}