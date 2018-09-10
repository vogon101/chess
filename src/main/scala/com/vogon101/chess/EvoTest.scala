package com.vogon101.chess

import java.io.File

import com.vogon101.chess.lib.Game
import com.vogon101.chess.lib.ai.minimax.{AlphaBetaAI, NNAlphaBetaAI}
import com.vogon101.chess.lib.ai.networks.NeuralNetwork
import com.vogon101.chess.lib.core.{Black, Board, Colour, White}
import breeze.linalg.{DenseMatrix, DenseVector, csvwrite}
import com.vogon101.chess.PlayTest.{game, playerBlack, playerWhite}

import scala.util.Random
import math.tanh

/**
  * EvoTest
  *
  * Created by fredd
  */
object EvoTest extends App {

  val size = 256 + 64
  val difficulty = 2
  val popSize = 16

  def evaluate(b: Board, c: Colour): Int = {
    val s = if (b.isCheckMate(c)) -1000
    else if (b.isCheckMate(c.otherColour)) 1000
    else b.pieces(c).map(_._1.value).sum - b.pieces(c.otherColour).map(_._1.value).sum
    val mod = if (b.isCheck(c)) -2 else if (b.isCheck(c.otherColour)) 2 else 0
    s + mod
  }

  class Player(var score: Double, var network: NeuralNetwork) {
    override def toString: String = s"Player(score=$score)"
  }

  var population: List[Player] = Range(0, popSize).map((n: Int) => {
    new Player(0 ,new NeuralNetwork(
      DenseMatrix.tabulate(size, size)((x,y) => Random.nextDouble() * 2 - 1d),
      DenseVector.tabulate(size)(x => Random.nextDouble() * 2 - 1d),
      DenseVector.tabulate(size)(x => Random.nextDouble() * 2 - 1d),
      x => tanh(x/2)
    ))
  }).toList

  var evolving = true

  val outputDir = "output/"

  var generation = 0
  while (evolving) {
    println(s"Generation $generation")
    for (round <- Range(0, 4)) {
      println(s"Round $round")
      population = Random.shuffle(population)
      val whites = population.take(popSize / 2)
      val blacks = population.drop(popSize / 2)

      var games = 0
      for (i <- Range(0, popSize / 2)) {
        games += 1
        println(s"Game $games")

        val players = List(new NNAlphaBetaAI(White, whites(i).network, difficulty), new NNAlphaBetaAI(Black, blacks(i).network, difficulty))

        val game = new Game()

        var running = true
        var j = 0

        var moves = 0

        while (running) {

          val p = players(j)

          val res = p.move(game)

          if (res._1) {

            if (res._2) {
              if (j == 0) {
                whites(i).score += 1
                blacks(i).score -= 1
              } else {
                whites(i).score -= 1
                blacks(i).score += 1
              }
              running = false
            }
            else if (game.board.isStalemate(p.colour.otherColour)) {
              running = false
            }

            moves += 1

            if (moves > 45) {
              running = false
              val eval = evaluate(game.board, White)
              if (eval > 0) {
                whites(i).score += 0.5
                blacks(i).score -= 0.5
              } else if (eval < 0) {
                whites(i).score -= 0.5
                blacks(i).score += 0.5
              }
            }

            //println(moves)
            //println(evaluate(game.board, White))
            //println(game.board.display())
            j = (j + 1) % 2
          }
        }
      }

    }

    population = population.sortBy(_.score).reverse

    println(population)

    val parents = population.take(4)

    population = parents.map(x => new Player(0, x.network)) ++ parents.flatMap(p => p.network.spawn(4).map(new Player(0, _)))

    val file = new File(outputDir + s"$generation")
    file.mkdirs()
    csvwrite(new File(outputDir + s"$generation/matrix.csv"), parents.head.network.connections)
    csvwrite(new File(outputDir + s"$generation/input.csv"), DenseMatrix(parents.head.network.inputWeights))
    csvwrite(new File(outputDir + s"$generation/output.csv"), DenseMatrix(parents.head.network.outputWeights))

    generation += 1

    val bestNetwork = parents.head.network
    val playerWhite = new NNAlphaBetaAI(White, bestNetwork, 1)
    val playerBlack = new AlphaBetaAI(Black, 1)

    val game = new Game()
    var running = true

    var moves = 0
    println(s"Best score: ${parents.head.score}")
    while(running) {

      val res1 = playerWhite.move(game)
      //println(res1)
      //println(game.board.display())
      //println("\n")
      running = !res1._2

      if (!running) println("Checkmate! - White wins")
      else if (game.board.isStalemate(Black)) {
        println("Stalemate")
        running = false
      } else {
        val res2 = playerBlack.move(game)
        //println(res2)
        //println(game.board.display())
        //println("\n")
        running = !res2._2

        if (!running) println("Checkmate - Black wins")
        else if (game.board.isStalemate(White)) {
          running  = false
          println("Stalemate")
        }
      }

      moves += 1
      if (moves > 150) {
        running = false
        println("Too many moves")
      }

    }

    println(game.board.display())


  }

}
