package com.farmersedge.aries.opencv
import org.opencv.core.{CvType, Mat}
import org.opencv.imgproc.Imgproc
import org.opencv.imgcodecs.Imgcodecs
object CloudMasking_MLP {

  def genCloudMask(tiffFile: String): Unit ={
    val bgr = Imgcodecs.imread(tiffFile)

    println(bgr)

    var gray: Mat = new Mat()
    Imgproc.cvtColor(bgr, gray, Imgproc.COLOR_BGR2GRAY)

    println(gray)


    var hsv: Mat = new Mat()
    Imgproc.cvtColor(bgr, hsv, Imgproc.COLOR_BGR2HSV)

    println(hsv)

    var rgb: Mat = new Mat()
    Imgproc.cvtColor(bgr, rgb, Imgproc.COLOR_BGR2RGB)

    println(rgb)

    var lab: Mat = new Mat()
    Imgproc.cvtColor(bgr, lab, Imgproc.COLOR_BGR2Lab)

    println(lab)

    var xyz: Mat = new Mat()
    Imgproc.cvtColor(bgr, xyz, Imgproc.COLOR_BGR2XYZ)

    println(xyz)

    var claheMat: Mat = new Mat()
    Imgproc.watershed(rgb, claheMat)
    val claheObj = Imgproc.createCLAHE()
    claheObj.apply(rgb, claheMat)

    println(claheMat)
    //CvType
  }
}
