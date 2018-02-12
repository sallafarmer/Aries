package com.farmersedge.aries.core

import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, ndvi, readTiff}
import geotrellis.raster.Tile

import scala.collection.mutable

object CloudMasking {

  def rgbToXYZ(sR: Int, sG: Int, sB: Int): (Double, Double, Double) = {

    val dR = sR / 255.0
    val dG = sG / 255.0
    val dB = sB / 255.0

    val r = if (dR <= 0.04045)
      dR / 12.92
    else
      Math.pow((dR + 0.055) / 1.055, 2.4)

    val g = if (dG <= 0.04045)
      dG / 12.92
    else
      Math.pow((dG + 0.055) / 1.055, 2.4)

    val b = if (dB <= 0.04045)
      dB / 12.92
    else
      Math.pow((dB + 0.055) / 1.055, 2.4)

    val X = r * 0.4124564 + g * 0.3575761 + b * 0.1804375
    val Y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750
    val Z = r * 0.0193339 + g * 0.1191920 + b * 0.9503041

    (X, Y, Z)
  }

  /**
    * Convert from RGB to CIELAB
    *
    * Implementation taken from:
    * http://ivrl.epfl.ch/research/superpixels
    *
    */
  def rgb2lab(sR: Int, sG: Int, sB: Int): (Double, Double, Double) = {

    // RGB to XYZ
    val (x, y, z) = rgbToXYZ(sR, sG, sB)

    // XYZ to LAB
    val epsilon = 0.008856 //actual CIE standard
    val kappa = 903.3 //actual CIE standard

    val Xr = 0.950456 //reference white
    val Yr = 1.0 //reference white
    val Zr = 1.088754 //reference white

    val xr = x / Xr
    val yr = y / Yr
    val zr = z / Zr

    val fx = if (xr > epsilon)
      Math.pow(xr, 1.0 / 3.0)
    else
      (kappa * xr + 16.0) / 116.0

    val fy = if (yr > epsilon)
      Math.pow(yr, 1.0 / 3.0)
    else
      (kappa * yr + 16.0) / 116.0

    val fz = if (zr > epsilon)
      Math.pow(zr, 1.0 / 3.0)
    else
      (kappa * zr + 16.0) / 116.0

    val L = 116.0 * fy - 16.0
    val A = 500.0 * (fx - fy)
    val B = 200.0 * (fy - fz)

    (L, A, B)
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

    val d = mapBands("red").toArray()
    val image_red = mapBands("red").toArray().map(_/65535)
    val chk = image_red.exists(x => x<0 || x > 1)

    val within = image_red.count(x => x>=0 && x <= 1)
    println("red")
    /*

  reference_image = original_bands['red'].copy()
  image_red = original_bands['red'].copy() / 65535.0
  image_red[np.isnan(image_red)] = 0

  if len(image_red[image_red<0])!=0 or len(image_red[image_red>1])!=0:
  status = 400
  error_message = "error: red band values are not between 0 and 65535!"
  return None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, status, error_message


  try:
  image_bgr = np.dstack((image_red, image_green, image_blue))
  ret_bgr = image_bgr.copy()
  # ret_bgr = exposure.adjust_log(ret_bgr)
  ret_bgr = exposure.equalize_adapthist(ret_bgr, clip_limit=0.04)

  hsv = rgb2hsv(image_bgr)
  image_h = hsv[:, :, 0]
  image_s = hsv[:, :, 1]
  image_v = hsv[:, :, 2]

  lab = rgb2lab(image_bgr)
  image_l = lab[:, :, 0]
  image_a = lab[:, :, 1]
  image_b = lab[:, :, 2]

  xyz = rgb2xyz(image_bgr)
  image_x = xyz[:, :, 0]
  image_y = xyz[:, :, 1]
  image_z = xyz[:, :, 2]

  clab = xyz2lab(xyz)
  image_cl = clab[:, :, 0]
  image_ca = clab[:, :, 1]
  image_cb = clab[:, :, 2]
   */
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
