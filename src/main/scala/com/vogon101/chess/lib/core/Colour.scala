package com.vogon101.chess.lib

/**
  * Colour
  *
  * Created by fredd
  */
sealed abstract class Colour (val name: String, val letter: String, val direction: Int){

  val otherColour: Colour

}

case object White extends Colour("White", "w", 1) {
  override val otherColour: Colour = Black
}
case object Black extends Colour("Black", "b", -1) {
  override val otherColour: Colour = White
}