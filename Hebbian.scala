/**
  *  Created by Nikhil Ashodariya
  *  visit http://codewithnikhil.blogspot.in/
  */

import scala.io._;

class Hebbian(number_Of_Epoch: Int, number_Of_Neuron: Int, number_Of_Input: Int, learning_Constant: Double) {
  private[this] var weight_Matrix: Array[Double] = _
  weight_Matrix = new Array[Double](number_Of_Neuron)
  var input_Vector = Array.ofDim[Double](number_Of_Neuron, number_Of_Input);
  var activation_Function = take_ActivationFunctionChoice()

  def take_Input() = {
    println("Enter data for weight of the neurons")
    for (i <- 0 until number_Of_Neuron) {
      println(s"Enter weight for ${i} th neuron ")
      weight_Matrix.update(i, StdIn.readDouble())
    }

    println("Reading input vector data")
    for (i <- 0 until number_Of_Input; j <- 0 until number_Of_Neuron) {
      println(s"Enter data for ${i} th neuron")
      println(s"Enter ${j + 1} weight ")
      input_Vector(j)(i) = StdIn.readDouble()
    }
  }

  def take_ActivationFunctionChoice() = {
    println("Select Activation Function")
    println("1. unipolar binary")
    println("2. bipolar binary")
    println("3. unipolar continuous")
    println("4. bipolar continuous")

    StdIn.readInt() match {
      case v: Int if (v >= 1 && v <= 4) => v
      case _ => -100000
    }
  }

  def calculate_ActivationValue(net: Double): Double = {
    activation_Function match {
      case 1 => if (net > 0) 1 else 0
      case 2 => if (net > 0) 1 else -1
      case 3 => (1.0 / (1 + Math.exp(-net)))
      case 4 => (2.0 / (1 + Math.exp(-net))) - 1
      case _ => -100000
    }
  }

  def calculateNet(j: Int) = {
    var temp: Double = 0.0
    for (k <- 0 until number_Of_Neuron) {
      temp = temp + weight_Matrix(k) * input_Vector(k)(j)
    }
    temp;
  }

  def train() = {
    val net = new Array[Double](number_Of_Neuron)
    val changeInWeight = new Array[Double](number_Of_Neuron)
    for (k <- 1 to number_Of_Epoch) {
      for (i <- 0 until number_Of_Input) {
        var temp = calculateNet(i)
        net(i) = temp
        println(s"net = ${temp}")
        val activationValue = calculate_ActivationValue(temp)
        temp = learning_Constant * activationValue
        for (j <- 0 until number_Of_Neuron) {
          changeInWeight(j) = temp * input_Vector(j)(i)
        }
        print("change in weight = ")
        changeInWeight.foreach((a: Double) => {
          print(s"${a}  ")
        })
        println()
        for (j <- 0 until number_Of_Neuron) {
          weight_Matrix(j) += changeInWeight(j)
        }
        print("changed weight = ")
        weight_Matrix.foreach((a: Double) => {
          print(s"${a}  ")
        })
        println();
        println()
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Enter number of neurons")
    val number_of_neurons = StdIn.readInt();
    println("Enter number of input")
    val number_of_input = StdIn.readInt();
    println("Enter number of learning Constant")
    val learning_constant = StdIn.readDouble();
    println("Enter number of epoch")
    val number_of_epoch = StdIn.readInt();

    val hebb = new Hebbian(number_of_epoch, number_of_neurons, number_of_input, learning_constant)
    hebb.take_Input()
    hebb.train()
  }
}