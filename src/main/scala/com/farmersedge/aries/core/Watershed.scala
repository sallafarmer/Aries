package com.farmersedge.aries.core

import java.awt.image.BufferedImage

import com.farmersedge.aries.core.ColorConversions._
import com.farmersedge.aries.watershed.WatershedSegmenter

import scala.collection.mutable.ArrayBuffer

case class Pixel(x: Int, y: Int, v: Double) {
 // val neighbours = new Vector[Int](8)

}

object Watershed {

  val queue = new ArrayBuffer[Pixel]()


  def sobel(grays: Array[GRAY]): Array[Array[Int]] = {
    //  out = np.sqrt(sobel_h(image, mask)**2 + sobel_v(image, mask)**2)
    //  out /= np.sqrt(2)
    null
  }

  def segment(gradient: Array[Array[Double]]): Unit ={

  }

  def toPixelsMatrix(in: Array[Double], shape: (Int, Int)): Array[Array[Pixel]] = {
    val mat = Array.ofDim[Pixel](shape._1, shape._2)

    for {
      r <- 0 until shape._1
      c <- 0 until shape._2
    } yield {
      val k = r * shape._2 + c
      mat(r)(c) = Pixel(r,c, in(k))
    }
    mat
  }
  import collection.JavaConverters._

  def run(rgbs: Array[RGB])(implicit shape: (Int, Int)): Array[Array[Int]] = {

    val grays = rgb2gray(rgbs).map(_.g.asInstanceOf[java.lang.Double])
  //  val pixels = toPixelsMatrix(grays, shape)
    //implement watershed algorithm now
//    val gradient = sobel(grays)
//    segments = watershed(gradient, markers = self.numOfSuperpixels, compactness = 0.001)

    val floodPoints = 200
    val windowWidth = shape._2
    val connectedPixels = 8

    val watershed = new WatershedSegmenter
    val start = System.currentTimeMillis
    val dstImage = watershed.calculate(grays.toList.asJava, shape._2, shape._1,
      floodPoints, windowWidth, connectedPixels)
    val end = System.currentTimeMillis
    // save the resulting image
    val totalms = end - start

    val d = dstImage.map(x => (x,1)).groupBy(_._1)
    Array.ofDim[Int](2,3)
  }
}
