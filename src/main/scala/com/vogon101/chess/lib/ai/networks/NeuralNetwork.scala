package com.vogon101.chess.lib.ai.networks

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.linalg._
import breeze.numerics._

import scala.util.Random

/**
  * Created by Freddie on 29/03/2017.
  */
class NeuralNetwork(val connections: DenseMatrix[Double], val inputWeights: DenseVector[Double], val outputWeights: DenseVector[Double], val activationFunction: (Double) => Double, val geneticAge: Int = 1) {

  assert(connections.cols == connections.rows)
  assert(inputWeights.length == connections.rows)

  var state: DenseVector[Double] = DenseVector.zeros(connections.cols)

  lazy val size = inputWeights.length

  def tick(in: DenseVector[Double]): Unit = {

    assert(in.length == connections.rows)

    //Inputs
    state = state + (inputWeights * in)

    //Progress the state
    state = connections * state
    state = state.map(activationFunction)

  }

  def output: DenseVector[Double] = state * outputWeights


  def reset():Unit = state = DenseVector.zeros(connections.cols)

  def spawn(n: Int, delta: Double = 20, spawnChance: Double = 0.3): List[NeuralNetwork] = {

    def nextNum(last: Double): Double = {
      if (Random.nextDouble() > spawnChance) last
      else {
        val n = last + (Random.nextGaussian() / delta) - 0.5/delta
        if (n < -1) -1
        else if (n > 1) 1
        else n
      }
    }

    (for (i <- 0 until n)
      yield new NeuralNetwork(
        connections.map(nextNum),
        inputWeights.map(nextNum),
        outputWeights.map(nextNum),
        activationFunction,
        geneticAge + 1
      )
      ).toList
  }

}

object NeuralNetwork {

  def cross(netA: NeuralNetwork, netB: NeuralNetwork): NeuralNetwork = {

    def splitAndCombineVector(lA: DenseVector[Double], lB: DenseVector[Double], points: Int = 1): DenseVector[Double] = {
      assert(lA.length == lB.length)
      val splitPoints = Range(0,points).map(X => Random.nextInt(lA.length - 5)).distinct.sorted ++ List(lA.length)
      val lists = List(lA, lB)
      var lasti = 0

      DenseVector.vertcat(splitPoints.zipWithIndex.map {
        case (point: Int, index: Int) =>
          val x = lists(index % 2).slice(lasti, point)
          lasti = point
          x
      }:_*)
    }

    def splitAndCombineMatrix(mA: DenseMatrix[Double], mB: DenseMatrix[Double]): DenseMatrix[Double] = {
      assert(mA.rows == mB.rows)
      new DenseMatrix[Double](mA.rows, mA.rows, splitAndCombineVector(DenseVector(mA.data), DenseVector(mB.data), mA.rows).toArray)
    }

    new NeuralNetwork(
      splitAndCombineMatrix(netA.connections, netB.connections),
      splitAndCombineVector(netA.inputWeights, netB.inputWeights, 10),
      splitAndCombineVector(netA.outputWeights, netB.outputWeights, 10),
      List(netA.activationFunction, netB.activationFunction)(Random.nextInt(2)),
      (netA.geneticAge + netB.geneticAge)/2 + 1
    ).spawn(1).head

  }

}
