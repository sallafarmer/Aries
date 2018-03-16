package com.farmersedge.aries.opencv

import com.farmersedge.aries.core.{GRAY, RGB}
import org.opencv.core.{Core, CvType, Mat, Scalar}
import org.opencv.imgproc.Imgproc

object Watershed {

  def sobel(grays: Array[GRAY]): Unit = {

  }

  def run(rgbs: Array[RGB]): Array[Array[Int]] = {

    //implement watershed algorithm now
//    gradient = sobel(rgb2gray(rgbs))
//    segments = watershed(gradient, markers = self.numOfSuperpixels, compactness = 0.001)
    null
  }

}
