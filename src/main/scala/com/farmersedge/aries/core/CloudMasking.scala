package com.farmersedge.aries.core

import com.farmersedge.aries.core.CloudMasking.add
import com.farmersedge.aries.core.ColorConversions._
import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, ndvi, readTiff}
import geotrellis.raster.Tile

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CloudMasking {

  case class ColorSpaceBands(shape0: Int,
                             shape1: Int,
                             claheRGB: Array[RGB],
                             reference_image: Array[Double],
                             image_red: Array[Double],
                             image_green: Array[Double],
                             image_blue: Array[Double],
                             image_nir: Array[Double],
                             image_h: Array[Double],
                             image_s: Array[Double],
                             image_v: Array[Double],
                             image_l: Array[Double],
                             image_a: Array[Double],
                             image_b: Array[Double],
                             image_x: Array[Double],
                             image_y: Array[Double],
                             image_z: Array[Double],
                             image_cl: Array[Double],
                             image_ca: Array[Double],
                             image_cb: Array[Double])

  def validateAndGet(bandLabel: String, mapBands: mutable.HashMap[String, Tile])
    : Either[String, Array[Double]] = {
    val orig = mapBands(bandLabel).toArray()
    val image_band = orig.map(x => if (x < 0) 0 else x / 65535.0)
    val chk = image_band.exists(x => x < 0 || x > 1)

    var error_message = ""
    if (chk) {
      error_message =
        s"Error: $bandLabel band values are not between 0 and 65535!"
      println(error_message)
      return Left(error_message)
    }
    val within = image_band.count(x => x >= 0 && x <= 1)
    println("red")

    Right(image_band)
  }

  def getAllColorSpaceBands(
      mapBands: mutable.HashMap[String, Tile]): Option[ColorSpaceBands] = {

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

    val reference_image = mapBands("red").toArray().map(_.toDouble)
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

    Some(
      ColorSpaceBands(
        rows,
        cols,
        claheRGB,
        reference_image,
        image_red,
        image_green,
        image_blue,
        image_nir,
        image_h,
        image_s,
        image_v,
        image_l,
        image_a,
        image_b,
        image_x,
        image_y,
        image_z,
        image_cl,
        image_ca,
        image_cb
      ))
  }

  def genCloudMask(tiffFile: String): Unit = {
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

    val cols = mapBands("red").cols
    val rows = mapBands("red").rows

    implicit val shape: (Int, Int) = (rows, cols)

    val optColorSpaceBands = getAllColorSpaceBands(mapBands)
    if (optColorSpaceBands.isEmpty)
      return

    val colorSpaceBands = optColorSpaceBands.get

    val cluster_list = new ArrayBuffer[Int]()

    val cluster_segments: Array[Array[Int]] =
      Watershed.run(colorSpaceBands.claheRGB)

    extractFeatures(cluster_segments, cluster_list.toArray, colorSpaceBands)

    println("done")
  }

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

  def extractData2(image: Array[Array[Int]],
                   indx0: Array[Int],
                   indx1: Array[Int]): Array[Int] = {

    val data = for {
      i <- indx0.zip(indx1)
    } yield {
      image(i._1)(i._2)
    }
    data.toArray
  }

  def extractData1(image: Array[Double], indx0: Array[Int], indx1: Array[Int])(
      implicit shape: (Int, Int)): Array[Double] = {

    val data = for {
      i <- indx0.zip(indx1)
    } yield {
      image(i._1 * shape._2 + i._2)
    }
    data
  }

  def isNaN(image: Array[Double],
            reference_image: Array[Double]): Array[Double] = {
    val outData = for {
      r <- reference_image.indices
      if reference_image(r) != Double.NaN
    } yield {
      image(r)
    }

    outData.toArray
  }

  def divide(arrayA: Array[Double], arrayB: Array[Double]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1 / x._2)
  }

  def divide(arrayA: Array[Int], arrayB: Array[Int]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1.toDouble / x._2.toDouble)
  }

  def add(arrayA: Array[Int], arrayB: Array[Int]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1.toDouble + x._2.toDouble)
  }

  def add(arrayA: Array[Int], arrayB: Array[Double]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1.toDouble + x._2.toDouble)
  }

  def add(arrayA: Array[Double], arrayB: Array[Double]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1.toDouble + x._2.toDouble)
  }

  def subtract(arrayA: Array[Double], arrayB: Array[Double]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1 - x._2)
  }

  def subtract(arrayA: Array[Int], arrayB: Array[Int]): Array[Double] = {
    arrayA.zip(arrayB).map(x => x._1.toDouble - x._2.toDouble)
  }

  def multiply(arrayA: Array[Double], d: Double): Array[Double] = {
    arrayA.map(_ * d)
  }

  def multiply(arrayA: Array[Int], d: Double): Array[Double] = {
    arrayA.map(_ * d)
  }

  def add(arrayA: Array[Double], d: Double): Array[Double] = {
    arrayA.map(_ + d)
  }

  def add(arrayA: Array[Int], d: Int): Array[Int] = {
    arrayA.map(_ + d)
  }

  def mean(arrayA: Array[Double]): Double = {
    arrayA.sum / arrayA.length
  }

  def mean(arrayA: Array[Int]): Double = {
    arrayA.sum.toDouble / arrayA.length.toDouble
  }

  def getMean(
      indx: Array[Array[Int]],
      image: Array[Double],
      reference_image: Array[Double])(implicit shape: (Int, Int)): Double = {
    val referenceData = extractData1(reference_image, indx(0), indx(1))
    val data = extractData1(image, indx(0), indx(1))
    val filtered_data = isNaN(data, referenceData)
    mean(filtered_data)
  }

  def getImageMean(image: Array[Double], reference_image: Array[Double])(
      implicit shape: (Int, Int)): Double = {
    val filtered_data = isNaN(image, reference_image)
    mean(filtered_data)
  }

  def get_SR(image_nir: Array[Double],
             image_red: Array[Double],
             indx: Array[Array[Int]],
             reference_image: Array[Double])(
      implicit shape: (Int, Int)): (Double, Array[Double], Array[Double]) = {
    val referenceData = extractData1(reference_image, indx(0), indx(1))
    val data_nir = extractData1(image_nir, indx(0), indx(1))
    val filtered_data_nir = isNaN(data_nir, referenceData)
    val data_red = extractData1(image_red, indx(0), indx(1))
    val filtered_data_red = isNaN(data_red, referenceData)
    (mean(divide(filtered_data_nir, add(filtered_data_red, 1))),
     filtered_data_nir,
     filtered_data_red)
  }

  def get_EVI(data_nir: Array[Double],
              data_red: Array[Double],
              image_blue: Array[Double],
              indx: Array[Array[Int]],
              reference_image: Array[Double])(
      implicit shape: (Int, Int)): (Double, Array[Double]) = {
    val referenceData = extractData1(reference_image, indx(0), indx(1))
    val data_blue = extractData1(image_blue, indx(0), indx(1))
    val filtered_data_blue = isNaN(data_blue, referenceData)
    val data_nir_1 = add(data_nir, 1)
    val a = subtract(data_nir, data_red)
    val b = subtract(multiply(data_red, 6.0), multiply(filtered_data_blue, 7.5))

    (mean(multiply(divide(a, add(add(data_nir_1, b), 1.0)), 2.5)),
     filtered_data_blue)

  }

  def get_CL_green(data_nir: Array[Double],
                   image_green: Array[Double],
                   indx: Array[Array[Int]],
                   reference_image: Array[Double])(
      implicit shape: (Int, Int)): (Double, Array[Double]) = {
    val referenceData = extractData1(reference_image, indx(0), indx(1))
    val data_green = extractData1(image_green, indx(0), indx(1))
    val filtered_data_green = isNaN(data_green, referenceData)
    (mean(divide(data_nir, add(add(filtered_data_green, 1), -1))),
     filtered_data_green)
  }
  def get_MTCI(data_nir: Array[Double],
               data_rededge: Array[Double],
               data_red: Array[Double])(implicit shape: (Int, Int)): Double = {
    mean(
      divide(subtract(data_nir, data_rededge),
             add(subtract(data_rededge, data_red), 1)))
  }

  def get_data_blue(image_blue: Array[Double],
                    indx: Array[Array[Int]],
                    reference_image: Array[Double])(
      implicit shape: (Int, Int)): Array[Double] = {
    val referenceData = extractData1(reference_image, indx(0), indx(1))
    val data_blue = extractData1(image_blue, indx(0), indx(1))
    val filtered_data_blue = isNaN(data_blue, referenceData)
    filtered_data_blue
  }

  def extractFeatures(cluster_segments: Array[Array[Int]],
                      cluster_list: Array[Int],
                      c: ColorSpaceBands)(implicit shape: (Int, Int)): Unit = {

    val i0 = Array(7, 8, 8, 9, 9, 10, 10, 10, 10)
    val i1 = Array(7, 7, 8, 7, 8, 7, 8, 9, 10)
    val cluster_indx = Array(i0, i1)

    val features = new mutable.HashMap[String, Double]()

    val ret = get_SR(c.image_nir, c.image_red, cluster_indx, c.reference_image)
    features("SR") = ret._1
    val data_nir = ret._2
    val data_red = ret._3

    val ret2 =
      get_EVI(data_nir, data_red, c.image_blue, cluster_indx, c.reference_image)

    features("EVI") = ret2._1
    var data_blue = ret2._2

    data_blue = get_data_blue(c.image_blue, cluster_indx, c.reference_image)
    val ret3 =
      get_CL_green(data_nir, c.image_green, cluster_indx, c.reference_image)
    features("CL_green") = ret3._1
    val data_green = ret3._2

    features("red_mean") = getMean(cluster_indx, c.image_red, c.reference_image)
    features("green_mean") =
      getMean(cluster_indx, c.image_green, c.reference_image)
    features("blue_mean") =
      getMean(cluster_indx, c.image_blue, c.reference_image)
    //  if source == 'RapidEye':
//    features['rededge_mean") = getMean(cluster_indx, c.image_rededge, c.reference_image)
    features("nir_mean") = getMean(cluster_indx, c.image_nir, c.reference_image)
//    features("ndvi_mean") = getMean(cluster_indx, c.image_ndvi, c.reference_image)
//    features("component0_mean") = getMean(cluster_indx, c.image_component0, c.reference_image)
//    features("component1_mean") = getMean(cluster_indx, c.image_component1, c.reference_image)
    features("h_mean") = getMean(cluster_indx, c.image_h, c.reference_image)
    features("s_mean") = getMean(cluster_indx, c.image_s, c.reference_image)
    features("v_mean") = getMean(cluster_indx, c.image_v, c.reference_image)
    features("l_mean") = getMean(cluster_indx, c.image_l, c.reference_image)
    features("a_mean") = getMean(cluster_indx, c.image_a, c.reference_image)
    features("b_mean") = getMean(cluster_indx, c.image_b, c.reference_image)
    features("x_mean") = getMean(cluster_indx, c.image_x, c.reference_image)
    features("y_mean") = getMean(cluster_indx, c.image_y, c.reference_image)
    features("z_mean") = getMean(cluster_indx, c.image_z, c.reference_image)
    features("cl_mean") = getMean(cluster_indx, c.image_cl, c.reference_image)
    features("ca_mean") = getMean(cluster_indx, c.image_ca, c.reference_image)
    features("cb_mean") = getMean(cluster_indx, c.image_cb, c.reference_image)

    features("image_mean_red") = getImageMean(c.image_red, c.reference_image)
    features("image_mean_green") =
      getImageMean(c.image_green, c.reference_image)
    features("image_mean_blue") = getImageMean(c.image_blue, c.reference_image)
    // if source == 'RapidEye':
    // features("image_mean_rededge") = getImageMean(c.image_rededge, c.reference_image)
    features("image_mean_nir") = getImageMean(c.image_nir, c.reference_image)
    //features("image_mean_ndvi") = getImageMean(c.image_ndvi, c.reference_image)
    //features("image_mean_component0") = getImageMean(image_component0, c.reference_image)
    //features("image_mean_component1") = getImageMean(image_component1, c.reference_image)
    features("image_mean_h") = getImageMean(c.image_h, c.reference_image)
    features("image_mean_s") = getImageMean(c.image_s, c.reference_image)
    features("image_mean_v") = getImageMean(c.image_v, c.reference_image)
    features("image_mean_l") = getImageMean(c.image_l, c.reference_image)
    features("image_mean_a") = getImageMean(c.image_a, c.reference_image)
    features("image_mean_b") = getImageMean(c.image_b, c.reference_image)
    features("image_mean_x") = getImageMean(c.image_x, c.reference_image)
    features("image_mean_y") = getImageMean(c.image_y, c.reference_image)
    features("image_mean_z") = getImageMean(c.image_z, c.reference_image)
    features("image_mean_cl") = getImageMean(c.image_cl, c.reference_image)
    features("image_mean_ca") = getImageMean(c.image_ca, c.reference_image)
    features("image_mean_cb") = getImageMean(c.image_cb, c.reference_image)

    features("normalized_R") = mean(
      divide(data_red, add(add(data_red, data_green), add(data_blue, 1.0))))
    features("normalized_G") = mean(
      divide(data_green, add(add(data_red, data_green), add(data_blue, 1.0))))
    features("normalized_B") = mean(
      divide(data_blue, add(add(data_red, data_green), add(data_blue, 1.0))))

    features("mean_R_by_B") = mean(divide(data_red, add(data_blue, 1.0)))
    // features("mean_R-B") = mean(subtract(data_red, data_blue))
    features("mean_R_by_B_plus_R") = mean(
      divide(data_red, add(data_blue, add(data_red, 1.0))))
    //  features("mean_chroma") = max(np.max(data_red), np.max(data_green), np.max(data_blue)) - \
    //  min(np.min(data_red), np.min(data_green), np.min(data_blue))

    features("R-G") = mean(subtract(data_red, data_green))
    features("R-B") = mean(subtract(data_red, data_blue))
    features("G-R") = mean(subtract(data_green, data_red))
    features("G-B") = mean(subtract(data_green, data_blue))
    features("B-R") = mean(subtract(data_blue, data_red))
    features("B-G") = mean(subtract(data_blue, data_green))

  }

}
