package com.vogon101.chess.lib.ai

import com.vogon101.chess.lib.core.{Board, Colour}
import com.vogon101.chess.lib.Game

/**
  * ChessAI
  *
  * Created by fredd
  */
abstract class ChessAI {

  val colour: Colour

  def move(game: Game): (Boolean, Boolean)

}

