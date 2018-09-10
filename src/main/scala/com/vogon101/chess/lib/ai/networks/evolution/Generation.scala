package com.vogon101.chess.lib.ai.networks.evolution

import breeze.linalg.DenseVector
import com.vogon101.chess.lib.ai.networks.NeuralNetwork


/**
  * Created by Freddie on 29/03/2017.
  */
class Generation(val genNumber: Int, val population: List[(NeuralNetwork, DenseVector[Double])], val fitnesses: DenseVector[Double], val averageFitness: Double) {


}
