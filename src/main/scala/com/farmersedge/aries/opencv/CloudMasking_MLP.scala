package com.farmersedge.aries.opencv
import org.opencv.core._
import org.opencv.imgproc.Imgproc
import org.opencv.imgcodecs.Imgcodecs

class WatershedSegmenter_opencv() {
  var markers = new Mat

  def setMarker(markerImage: Mat): Unit = {
    markerImage.convertTo(markers, CvType.CV_32S)
  }

  def process(image: Mat): Mat = {
    Imgproc.watershed(image, markers)
    markers.convertTo(markers, CvType.CV_8U)
    markers
  }
}


object CloudMasking_MLP {

  def genCloudMask(tiffFile: String): Unit ={
    val bgr = Imgcodecs.imread(tiffFile)

    println(bgr)

    var gray: Mat = new Mat()
    Imgproc.cvtColor(bgr, gray, Imgproc.COLOR_BGR2GRAY)

    println(gray)


 //   var markers1: Mat = new Mat()
 //   gray.convertTo(markers1, CvType.CV_32SC1)
    val markers1 = new Mat(bgr.size, CvType.CV_32SC1, new Scalar(0))

    Imgproc.watershed(bgr, markers1)

    var markers2: Mat = new Mat()
    markers1.convertTo(markers2, CvType.CV_8U)

    val rows = markers2.rows()
    val cols = markers2.cols()

    val d = for {
      r <- 0 until rows
      c <- 0 until cols
      if markers2.get(r, c).head > 0.0
    } yield {
      markers2.get(r, c)
    }

    var rgba: Mat = new Mat()
    Imgproc.cvtColor(bgr, rgba, Imgproc.COLOR_BGR2BGRA)



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

    gray.convertTo(claheMat, CvType.CV_32SC1)

    val threeChannel = new Mat
    import org.opencv.imgproc.Imgproc
    Imgproc.threshold(threeChannel, threeChannel, 100, 255, Imgproc.THRESH_BINARY)

    val fg = new Mat(rgba.size, CvType.CV_8U)
    Imgproc.erode(threeChannel, fg, new Mat(), new Point(-1, -1), 2)

    val bg = new Mat(rgba.size, CvType.CV_8U)
    Imgproc.dilate(threeChannel, bg, new Mat(), new Point(-1, -1), 3)
    Imgproc.threshold(bg, bg, 1, 128, Imgproc.THRESH_BINARY_INV)

    val markers = new Mat(rgba.size, CvType.CV_8U, new Scalar(0))
    Core.add(fg, bg, markers)

    val segmenter = new WatershedSegmenter_opencv
    segmenter.setMarker(markers)
    val result = segmenter.process(rgba)


    val claheObj = Imgproc.createCLAHE()
    claheObj.apply(rgb, claheMat)

    println(claheMat)
    //CvType
  }
}
