package com.farmersedge.aries.core

object Util {
  def to2DMatrix(in: Array[Double], shape: (Int, Int)): Array[Array[Double]] = {
    val mat = Array.ofDim[Double](shape._1, shape._2)

    for {
      r <- 0 until shape._1
      c <- 0 until shape._2
    } yield {
      val k = r * shape._2 + c
      mat(r)(c) = in(k)
    }
    mat
  }
}
