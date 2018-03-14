package com.farmersedge.aries.core

import com.farmersedge.aries.core.ColorConversions._
import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, ndvi, readTiff}
import com.farmersedge.aries.opencv.Watershed
import geotrellis.raster.Tile

import scala.collection.mutable

object CloudMasking {

  case class ColorSpaceBands(shape0:Int, shape1: Int,
                             claheRGB:Array[RGB], reference_image:Array[Int],
                             image_red:Array[Double],
                             image_green:Array[Double],
                             image_blue:Array[Double], image_nir:Array[Double],
                              image_h:Array[Double],
                             image_s:Array[Double], image_v:Array[Double],
                             image_l:Array[Double], image_a:Array[Double],
                             image_b:Array[Double], image_x:Array[Double],
                             image_y:Array[Double],
                              image_z:Array[Double],
                             image_cl:Array[Double], image_ca:Array[Double],
                             image_cb:Array[Double])

  def validateAndGet(bandLabel: String, mapBands: mutable.HashMap[String, Tile]): Either[String, Array[Double]] = {
    val image_band = mapBands(bandLabel).toArray().map(_/65535.0)
    val chk = image_band.exists(x => x<0 || x > 1)

    var error_message = ""
    if (chk) {
      error_message = s"Error: $bandLabel band values are not between 0 and 65535!"
      println(error_message)
      return Left(error_message)
    }
    val within = image_band.count(x => x>=0 && x <= 1)
    println("red")

    Right(image_band)
  }

  def getAllColorSpaceBands(mapBands: mutable.HashMap[String, Tile]): Option[ColorSpaceBands] = {

    var bands: Array[String] = null
    var status = 0


      bands = Array[String](
        "red",
        "green",
        "blue",
        "nir",
        "ndvi",
        "component0",
        "component1"
      )
      status = 200

    var error_message = "error: missing the following band(s): "

    val cols = mapBands("red").cols
    val rows = mapBands("red").rows

    val reference_image = mapBands("red").toArray()
    var ret = validateAndGet("red", mapBands)
    if (ret.isLeft) {
      return None
    }
    val image_red = ret.right.get

    ret = validateAndGet("blue", mapBands)
    if (ret.isLeft) {
      return None
    }
    val image_blue = ret.right.get

    ret = validateAndGet("green", mapBands)
    if (ret.isLeft) {
      return None
    }
    val image_green = ret.right.get

    ret = validateAndGet("nir", mapBands)
    if (ret.isLeft) {
      return None
    }
    val image_nir = ret.right.get

/*
    val h1 = HSV(0.5985636453689945,0.20375416288222822,0.41592757966848126)
    val r1 = hsv2rgb(h1)
    val h2 = rgb2hsv(r1)
*/
    val rgbs = makeRGBArray(image_red, image_green, image_blue)

  /*  val hsv = rgb2hsv(rgbs.head)
    val lab = rgb2lab(rgbs.head)
    val xyz = rgb2xyz(rgbs.head)
    val clab = xyz2lab(xyz)
*/
    val hsvs = rgb2hsv(rgbs)
    val image_h = hsvs.map(_.h).toArray
    val image_s = hsvs.map(_.s).toArray
    val image_v = hsvs.map(_.v).toArray

    val labs = rgb2lab(rgbs)
    val image_l = labs.map(_.l).toArray
    val image_a = labs.map(_.a).toArray
    val image_b = labs.map(_.b).toArray

    val xyzs = rgb2xyz(rgbs)
    val image_x = xyzs.map(_.x).toArray
    val image_y = xyzs.map(_.y).toArray
    val image_z = xyzs.map(_.z).toArray

    val clabs = xyz2lab(xyzs)
    val image_cl = clabs.map(_.l).toArray
    val image_ca = clabs.map(_.a).toArray
    val image_cb = clabs.map(_.b).toArray

    val claheRGB = Clahe.run(hsvs, (rows, cols))

    Some(ColorSpaceBands(rows, cols, claheRGB, reference_image, image_red, image_green, image_blue, image_nir,
      image_h, image_s, image_v, image_l, image_a, image_b, image_x, image_y,
      image_z, image_cl, image_ca, image_cb))
  }

  def genCloudMask(tiffFile: String): Unit ={
    val original = readTiff(tiffFile)

    val red_band = original.raster.tile.band(bandsToIndexes('r))
    val green_band = original.raster.tile.band(bandsToIndexes('g))
    val blue_band = original.raster.tile.band(bandsToIndexes('b))

    val nir_band = original.raster.tile.band(bandsToIndexes('nir))

    val ndvi_band = ndvi(red_band, nir_band)

    val mapBands = new mutable.HashMap[String, Tile]()
    mapBands("red") = red_band
    mapBands("green") = green_band
    mapBands("blue") = blue_band
    mapBands("nir") = nir_band
    mapBands("ndvi") = ndvi_band

    val optColorSpaceBands = getAllColorSpaceBands(mapBands)
    if (optColorSpaceBands.isEmpty)
      return

    val colorSpaceBands = optColorSpaceBands.get

 //   val clusters = Watershed.run(colorSpaceBands.claheRGB)

    println("done")
  }


/*


  def getImageMean(image, reference_image):Double = {
  image = image[~np.isnan(reference_image)]
  return np.mean(image)

  def get_SR(image_nir, image_red, indx, reference_image):Double = {
  referenceData = reference_image[indx[0], indx[1]]
  data_nir = image_nir[indx[0], indx[1]]
  data_nir = data_nir[~np.isnan(referenceData)]
  # data_nir = data_nir[np.nonzero(data_nir)]
  data_red = image_red[indx[0], indx[1]]
  data_red = data_red[~np.isnan(referenceData)]
  # data_red = data_red[np.nonzero(data_red)]
  return np.mean(np.divide(data_nir, data_red + 1)), data_nir, data_red

  def get_EVI(data_nir, data_red, image_blue, indx, reference_image):Double = {
  referenceData = reference_image[indx[0], indx[1]]
  data_blue = image_blue[indx[0], indx[1]]
  data_blue = data_blue[~np.isnan(referenceData)]
  # data_blue = data_blue[np.nonzero(data_blue)]
  return np.mean(2.5 * np.divide((data_nir - data_red), (1.0 + data_nir + (6.0 * data_red) - (7.5 * data_blue)) + 1.0)), data_blue

  def get_CL_green(data_nir, image_green, indx, reference_image):Double = {
  referenceData = reference_image[indx[0], indx[1]]
  data_green = image_green[indx[0], indx[1]]
  data_green = data_green[~np.isnan(referenceData)]
  # data_green = data_green[np.nonzero(data_green)]
  return np.mean(np.divide(data_nir, data_green + 1.0) - 1.0), data_green

  def get_MTCI(data_nir, data_rededge, data_red):Double = {
  return np.mean(np.divide((data_nir - data_rededge),(data_rededge - data_red) + 1.0))

  def get_data_blue(image_blue, indx, reference_image):Double = {
  referenceData = reference_image[indx[0], indx[1]]
  data_blue = image_blue[indx[0], indx[1]]
  data_blue = data_blue[~np.isnan(referenceData)]
  return data_blue

*/

  def toMatrix(in: Array[Int], shape: (Int, Int)): Array[Array[Int]] = {
    val mat = Array.ofDim[Int](shape._1, shape._2)

    for {
      r <- 0 until shape._1
      c <- 0 until shape._2
    } yield {
      val k = r * shape._2 + c
      mat(r)(c) = in(k)
    }
    mat
  }

  def referenceData(image: Array[Array[Int]],
                    indx0: Array[Int], indx1: Array[Int]): Array[Int] = {

    val data = for {
      i <- indx0.zip(indx1)
    } yield {
      image(i._1)(i._2)
    }
    data.toArray
  }

  def getMean(indx: Array[Array[Int]], image: Array[Int], reference_image:Array[Array[Int]]): Double = {
    val referenceData = referenceData(reference_image, indx(0), indx(1))
    val data = referenceData(image, indx(0), indx(1))
    //  data = data[~np.isnan(referenceData)]
    //  return np.mean(data)
    data
  }

  def extractFeatures(colorSpaceBands: ColorSpaceBands): Unit = {

    val reference_image = toMatrix(colorSpaceBands.reference_image,
      (colorSpaceBands.shape0, colorSpaceBands.shape1))

  }

}
