package com.vogon101.chess

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector, csvread}
import com.vogon101.chess.lib.Game
import com.vogon101.chess.lib.ai.minimax.{AlphaBetaAI, NNAlphaBetaAI}
import com.vogon101.chess.lib.ai.networks.NeuralNetwork
import com.vogon101.chess.lib.core.{Black, White}

import scala.math.tanh

object PlayTest extends App {

  val outputDir = "output/64"

  val connections = csvread(new File(outputDir + "/matrix.csv"))
  val input = csvread(new File(outputDir + "/input.csv")).t.apply(::, 0)
  val output = csvread(new File(outputDir + "/output.csv")).t.apply(::, 0)

  val network = new NeuralNetwork(connections, input, output, x => tanh(x/2))

  val playerWhite = new NNAlphaBetaAI(White, network, 1)
  val playerBlack = new AlphaBetaAI(Black, 2)

  val game = new Game()
  var running = true

  while(running) {

    val res1 = playerWhite.move(game)
    println(res1)
    println(game.board.display())
    println("\n")
    running = !res1._2

    if (!running) println("Checkmate! - White wins")


    val res2 = playerBlack.move(game)
    println(res2)
    println(game.board.display())
    println("\n")
    running = !res2._2

    if (!running) println("Checkmate - Black wins")

  }

}
