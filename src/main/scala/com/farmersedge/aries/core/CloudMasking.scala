package com.farmersedge.aries.core

import com.farmersedge.aries.core.ColorConversions.makeRGBArray
import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, ndvi, readTiff}
import com.farmersedge.aries.opencv.Watershed
import geotrellis.raster.Tile

import scala.collection.mutable

object CloudMasking {

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

  def getAllColorSpaceBands(mapBands: mutable.HashMap[String, Tile]): Unit = {

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
      return
    }
    val red_band = ret.right.get

    ret = validateAndGet("blue", mapBands)
    if (ret.isLeft) {
      return
    }
    val blue_band = ret.right.get

    ret = validateAndGet("green", mapBands)
    if (ret.isLeft) {
      return
    }
    val green_band = ret.right.get

    ret = validateAndGet("nir", mapBands)
    if (ret.isLeft) {
      return
    }
    val image_nir = ret.right


    val rgbs = ColorConversions.makeRGBArray(red_band, green_band, blue_band)

  /*  val hsv = ColorConversions.rgb2hsv(rgbs.head)
    val lab = ColorConversions.rgb2lab(rgbs.head)
    val xyz = ColorConversions.rgb2xyz(rgbs.head)
    val clab = ColorConversions.xyz2lab(xyz)
*/
    val hsvs = ColorConversions.rgb2hsv(rgbs)

    val labs = ColorConversions.rgb2lab(rgbs)
    val xyzs = ColorConversions.rgb2xyz(rgbs)
    val clabs = ColorConversions.xyz2lab(xyzs)

    val claheRGB = Clahe.run(hsvs, (rows, cols))

//    val clusters = Watershed.run(claheRGB)

    println("done")
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

    getAllColorSpaceBands(mapBands)


  }

}
