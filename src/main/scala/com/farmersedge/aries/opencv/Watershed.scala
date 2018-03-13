package com.farmersedge.aries.opencv

import com.farmersedge.aries.core.{GRAY, RGB}
import org.opencv.core.{Core, CvType, Mat, Scalar}
import org.opencv.imgproc.Imgproc

object Watershed {



  def sobel(grays: Array[GRAY]): Unit = {

  }

  def run(rgbs: Array[RGB]): Unit = {

    //implement watershed algorithm now
//    gradient = sobel(rgb2gray(rgbs))
//    segments = watershed(gradient, markers = self.numOfSuperpixels, compactness = 0.001)
  }

  def main(args: Array[String]): Unit = {
    System.out.println("Welcome to OpenCV " )
    val m = new Mat(5, 10, CvType.CV_8UC1, new Scalar(0))
    val m2 = new Mat(5, 10, CvType.CV_8UC1, new Scalar(0))
    System.out.println("OpenCV Mat: " + m)
    val mr1 = m.row(1)
    mr1.setTo(new Scalar(1))
    val mc5 = m.col(5)
    mc5.setTo(new Scalar(5))

    Imgproc.cvtColor(m, m2, Imgproc.COLOR_RGB2HSV)
    val clahe = Imgproc.createCLAHE()
    clahe.apply(m, mc5)

    System.out.println("OpenCV Mat data:\n" + m.dump)
  }

//    try System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

}
