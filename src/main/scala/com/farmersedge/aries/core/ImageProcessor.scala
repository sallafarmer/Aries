package com.farmersedge.aries.core

import geotrellis.raster.io.geotiff.{MultibandGeoTiff, SinglebandGeoTiff}
import geotrellis.raster.io.geotiff.reader.GeoTiffReader

object ImageProcessor {

  def process(path: String): Unit = {

    println(path)

    val geoTiff: SinglebandGeoTiff = GeoTiffReader.readSingleband(path)
    println(geoTiff)

    println()

    val geoTiff2: MultibandGeoTiff = GeoTiffReader.readMultiband(path)
    println(geoTiff2)
    println()

  }
}
