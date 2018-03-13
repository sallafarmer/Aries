package com.farmersedge.aries.core

import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, readTiff}

import scala.collection.mutable.ArrayBuffer

case class RGB(r: Double, g: Double, b: Double)
case class GRAY(g: Double)
case class HSV(h: Double, s: Double, v: Double)
case class LAB(l: Double, a: Double, b: Double)
case class XYZ(x: Double, y: Double, z: Double)

object ColorConversions {

  def rgb2hsv(rgb: RGB): HSV = {
    var h = 0.0
    var s = 0.0
    var v = 0.0
    var min = 0.0
    var max = 0.0
    var delta = 0.0

    min = if (rgb.r < rgb.g) rgb.r else rgb.g
    min = if (min < rgb.b) min else rgb.b

    max = if (rgb.r > rgb.g) rgb.r else rgb.g
    max = if (max > rgb.b) max else rgb.b

    v = max // v is set here

    delta = max - min
    if (delta < 0.00001) {
      s = 0
      h = 0 // undefined, maybe nan?

      return HSV(h, s, v)
    }

    if (max > 0.0) { // NOTE: if Max is == 0, this divide would cause a crash
      s = delta / max // s

    } else { // if max is 0, then r = g = b = 0
      // s = 0, h is undefined
      s = 0.0
      h = Double.NaN // its now undefined

      return HSV(h, s, v)
    }

    if (rgb.r >= v) { // > is bogus, just keeps compiler happy
      h = (rgb.g - rgb.b) / delta // between yellow & magenta
    } else if (rgb.g >= v) {
      h = 2.0 + (rgb.b - rgb.r) / delta // between cyan & yellow
    } else {
      h = 4.0 + (rgb.r - rgb.g) / delta // between magenta & cyan
    }

    h /= 6

    HSV(h, s, v)
  }

  def hsv2rgb(hsv: HSV): RGB = {
    var hh = .0
    var p = .0
    var q = .0
    var t = .0
    var ff = .0
    var i = 0L
    var r = .0
    var g = .0
    var b = .0


    if (hsv.s <= 0.0) { // < is bogus, just shuts up warnings
      r = hsv.v
      g = hsv.v
      b = hsv.v
      return RGB(r,g,b)
    }
    hh = hsv.h
    if (hh >= 360.0) hh = 0.0
    hh /= 60.0
    i = hh.toLong
    ff = hh - i
    p = hsv.v * (1.0 - hsv.s)
    q = hsv.v * (1.0 - (hsv.s * ff))
    t = hsv.v * (1.0 - (hsv.s * (1.0 - ff)))
    i match {
      case 0 =>
        r = hsv.v
        g = t
        b = p
      case 1 =>
        r = q
        g = hsv.v
        b = p
      case 2 =>
        r = p
        g = hsv.v
        b = t
      case 3 =>
        r = p
        g = q
        b = hsv.v

      case 4 =>
        r = t
        g = p
        b = hsv.v

      case 5 | _ =>
        r = hsv.v
        g = p
        b = q

    }
    RGB(r,g,b)
  }

  def rgb2xyz(rgb: RGB): XYZ ={
    var r = rgb.r
    var g = rgb.g
    var b = rgb.b

    r = if (r > 0.04045) Math.pow((r + 0.055) / 1.055, 2.4) else r / 12.92
    g = if (g > 0.04045) Math.pow((g + 0.055) / 1.055, 2.4) else g / 12.92
    b = if (b > 0.04045) Math.pow((b + 0.055) / 1.055, 2.4) else b / 12.92

    var x = r * 0.4124 + g * 0.3576 + b * 0.1805
    var y = r * 0.2126 + g * 0.7152 + b * 0.0722
    var z = r * 0.0193 + g * 0.1192 + b * 0.9505

    XYZ(x, y, z)
  }

  def xyz2rgb(xyz: XYZ): RGB={
    val x = xyz.x
    val y = xyz.y
    val z = xyz.z

    var r = x *  3.2406 + y * -1.5372 + z * -0.4986
    var g = x * -0.9689 + y *  1.8758 + z *  0.0415
    var b = x *  0.0557 + y * -0.2040 + z *  1.0570

    r = if (r > 0.0031308) (1.055 * Math.pow(r, 1/2.4) - 0.055) else 12.92 * r
    g = if (g > 0.0031308) (1.055 * Math.pow(g, 1/2.4) - 0.055) else 12.92 * g
    b = if (b > 0.0031308) (1.055 * Math.pow(b, 1/2.4) - 0.055) else 12.92 * b

    RGB(r,g,b)
 /*   RGB(Math.max(0, Math.min(1, r)),
      Math.max(0, Math.min(1, g)),
      Math.max(0, Math.min(1, b)))
      */
  }

  def xyz2lab(xyz: XYZ): LAB ={
    var x = xyz.x / 0.95047
    var y = xyz.y / 1.00000
    var z = xyz.z / 1.08883


    x = if (x > 0.008856) Math.pow(x, 1.0/3.0) else (7.787 * x) + 16.0/116.0
    y = if (y > 0.008856) Math.pow(y, 1.0/3.0) else (7.787 * y) + 16.0/116.0
    z = if (z > 0.008856) Math.pow(z, 1.0/3.0) else (7.787 * z) + 16.0/116.0

    LAB((116 * y) - 16, 500 * (x - y), 200 * (y - z))
  }

  def lab2xyz(lab: LAB): XYZ = {

    var y = (lab.l + 16) / 116
    var x = lab.a / 500 + y
    var z = y - lab.b / 200

    x = 0.95047 * (if (x > 0.2068966) x * x * x else (x - 16/116) / 7.787)
    y = 1.00000 * (if (y  > 0.2068966) y * y * y else (y - 16/116) / 7.787)
    z = 1.08883 * (if (z  > 0.2068966) z * z * z else  (z - 16/116) / 7.787)

    XYZ(x,y,z)


  }

  def rgb2gray(rgb: RGB): GRAY ={
    GRAY(0.2125 * rgb.r + 0.7154 * rgb.g + 0.0721 * rgb.b)
  }


  def rgb2lab(rgb: RGB): LAB ={
    val xyz = rgb2xyz(rgb)
    xyz2lab(xyz)
  }

  def lab2rgb(lab: LAB): RGB = {
    val xyz = lab2xyz(lab)
    xyz2rgb(xyz)
  }

  def test(rgb: RGB): Unit = {
    val hsv = rgb2hsv(rgb)
    val rgb1 = hsv2rgb(hsv)
    //  println(rgb + " -> " + hsv + " -> " + rgb2)

    val xyz = rgb2xyz(rgb)
    val rgb2 = xyz2rgb(xyz)
 //   println(rgb + " -> " + xyz + " -> " + rgb2)


    val lab = rgb2lab(rgb)
    val rgb3 = lab2rgb(lab)
    println(rgb + " -> " + lab + " -> " + rgb3)
  }


  def makeRGBArray(red: Array[Double], green: Array[Double], blue: Array[Double]): Array[RGB] ={
    val n = red.length
    val rgbs = for {
      i <- 0 until n
    } yield {
     // test(RGB(red(i), green(i), blue(i)))
      RGB(red(i), green(i), blue(i))
    }
    rgbs.toArray
  }

  def rgb2hsv(rgbs: Array[RGB]): Array[HSV] ={
    val hsvs = for { rgb <- rgbs}
      yield {
        val hsv = rgb2hsv(rgb)
        val rgb2 = hsv2rgb(hsv)
        if (Math.abs(rgb.r - rgb2.r) > 0.2 || Math.abs(rgb.g - rgb2.g) > 0.2 || Math.abs(rgb.b - rgb2.b) > 0.2) {
          println("rgb2hsv: " + rgb + " -> " + hsv + " -> " + rgb2)

        }
        hsv
      }
    hsvs
  }

  def hsv2rgb(hsvs: Array[HSV]): Array[RGB] ={
    val rgbs = for { hsv <- hsvs}
      yield {
        val rgb = hsv2rgb(hsv)
        val hsv2 = rgb2hsv(rgb)
        if (Math.abs(hsv.h - hsv2.h) > 0.2 || Math.abs(hsv.s - hsv2.s) > 0.2 || Math.abs(hsv.v - hsv2.v) > 0.2) {
          println("hsv2rgb: " + hsv + " -> " + rgb + " -> " + hsv2)

        }
        rgb
      }
    rgbs
  }


  def rgb2xyz(rgbs: Array[RGB]): Array[XYZ] ={
    val xyzs = for { rgb <- rgbs}
      yield {
        val xyz = rgb2xyz(rgb)
        val rgb2 = xyz2rgb(xyz)
        if (Math.abs(rgb.r - rgb2.r) > 0.2 || Math.abs(rgb.g - rgb2.g) > 0.2 || Math.abs(rgb.b - rgb2.b) > 0.2) {
          println("rgb2xyz: " + rgb + " -> " + xyz + " -> " + rgb2)

        }
        xyz
      }
    xyzs
  }

  def rgb2lab(rgbs: Array[RGB]): Array[LAB] ={

    val labs = for { rgb <- rgbs}
      yield {
        val lab = rgb2lab(rgb)
        val rgb2 = lab2rgb(lab)

        if (Math.abs(rgb.r - rgb2.r) > 0.2 || Math.abs(rgb.g - rgb2.g) > 0.2 || Math.abs(rgb.b - rgb2.b) > 0.2) {
          println("rgb2lab: " + rgb + " -> " + lab + " -> " + rgb2)

        }
        lab
      }
    labs
  }

  def xyz2lab(xyzs: Array[XYZ]): Array[LAB] ={

    val labs = for { xyz <- xyzs}
      yield {
        val lab = xyz2lab(xyz)
        val xyz2 = lab2xyz(lab)

        if (Math.abs(xyz.x - xyz2.x) > 0.2 || Math.abs(xyz.y - xyz2.y) > 0.2 || Math.abs(xyz.z - xyz2.z) > 0.2) {
          println("xyz2lab: " + xyz + " -> " + lab + " -> " + xyz2)

        }
        lab
      }
    labs
  }

  def rgb2gray(rgbs: Array[RGB]): Array[GRAY] ={

    val grays = for { rgb <- rgbs}
      yield {
        val gray = rgb2gray(rgb)

        gray
      }
    grays
  }


  def main(args: Array[String]): Unit = {

    var tiffFile = s"data/20170217_215326_0c22_1B_AnalyticMS_274501.tiff"

    val original = readTiff(tiffFile)

    val red_band = original.raster.tile.band(bandsToIndexes('r))
    val green_band = original.raster.tile.band(bandsToIndexes('g))
    val blue_band = original.raster.tile.band(bandsToIndexes('b))

    val nir_band = original.raster.tile.band(bandsToIndexes('nir))

    val rgbs = makeRGBArray(red_band.toArrayDouble(), green_band.toArrayDouble(), blue_band.toArrayDouble())

  }

}
