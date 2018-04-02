package com.farmersedge.aries.core

import java.io.InputStreamReader
import java.nio.{ByteBuffer, DoubleBuffer, FloatBuffer, IntBuffer}

import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, readTiff}
import geotrellis.raster.io.geotiff.MultibandGeoTiff
import geotrellis.raster.{
  ArrayTile,
  DoubleCellType,
  Tile,
  UShortArrayTile,
  UShortCellType
}
import jep.{DirectNDArray, Jep, NDArray}

import scala.collection.mutable

object CloudMaskingJep {

  def getDirectNDArray(band: Tile): DirectNDArray[DoubleBuffer] = {
    val rows = band.rows
    val cols = band.cols

    val band_buffer: DoubleBuffer =
      ByteBuffer.allocateDirect(rows * cols * 8).asDoubleBuffer()
    band_buffer.put(band.toArrayDouble(), 0, rows * cols)
    val band_data = new DirectNDArray[DoubleBuffer](band_buffer)
    band_data
  }

  def process(source: String, originalTiff: MultibandGeoTiff)
             (implicit jep: Jep): (Tile, Tile, MultibandGeoTiff) = {
    var stTime = System.currentTimeMillis()
    var endTime = System.currentTimeMillis()

    //PlanetScope or RapidEye
    jep.eval(
      s"source = {'AcquisitionDate': datetime.datetime(2017,8,2),'service': '$source'}")

    val red_band = originalTiff.raster.tile.band(bandsToIndexes('r))
    val green_band = originalTiff.raster.tile.band(bandsToIndexes('g))
    val blue_band = originalTiff.raster.tile.band(bandsToIndexes('b))
    val nir_band = originalTiff.raster.tile.band(bandsToIndexes('nir))

    val rows = red_band.rows
    val cols = red_band.cols

    val red_band_data = getDirectNDArray(red_band)
    val green_band_data = getDirectNDArray(green_band)
    val blue_band_data = getDirectNDArray(blue_band)
    val nir_band_data = getDirectNDArray(nir_band)

    jep.set("rows", rows)
    jep.set("cols", cols)
    jep.set("red_band_data", red_band_data)
    jep.set("green_band_data", green_band_data)
    jep.set("blue_band_data", blue_band_data)
    jep.set("nir_band_data", nir_band_data)

    jep.eval("bands = dict()")
    jep.eval("bands[\"red\"] = red_band_data.reshape(rows, cols)")
    jep.eval("bands[\"green\"] = green_band_data.reshape(rows, cols)")
    jep.eval("bands[\"blue\"] = blue_band_data.reshape(rows, cols)")
    jep.eval("bands[\"nir\"] = nir_band_data.reshape(rows, cols)")

    if (source == "RapidEye") {
      val redegde_band = originalTiff.raster.tile.band(bandsToIndexes('rededge))
      val redegde_band_data = getDirectNDArray(redegde_band)
      jep.set("redegde_band_data", redegde_band_data)
      jep.eval("bands[\"redegde\"] = redegde_band_data.reshape(rows, cols)")
    }

    jep.eval("cloudMaskGenerator = SegmentationBasedCloudMask()")

    val src = jep.getValue("source")
    jep.eval("cloudMask = cloudMaskGenerator.generateMask(bands, source)")

    val cloudMask = jep.getValue("cloudMask")
    val shadowLayer = jep.getValue("cloudMask.shadowLayer")
    val cloudLayer = jep.getValue("cloudMask.cloudLayer")

    endTime = System.currentTimeMillis()
    println(s"ran cloudmasking script in ${endTime - stTime} msecs")
    stTime = System.currentTimeMillis()

    val shadowLayerMaskArray = shadowLayer.asInstanceOf[NDArray[Array[Boolean]]]

    val shadowLayerMaskArrayShort = for {
      i <- shadowLayerMaskArray.getData.indices
    } yield {
      if (shadowLayerMaskArray.getData()(i))
        1.toShort
      else
        0.toShort
    }
    val shadowLayerMaskTile = UShortArrayTile
      .apply(shadowLayerMaskArrayShort.toArray,
             shadowLayerMaskArray.getDimensions.tail.head,
             shadowLayerMaskArray.getDimensions.head)
      .convert(DoubleCellType)

    val cloudLayerMaskArray = cloudLayer.asInstanceOf[NDArray[Array[Boolean]]]

    val cloudLayerMaskArrayShort = for {
      i <- shadowLayerMaskArray.getData.indices
    } yield {
      if (cloudLayerMaskArray.getData()(i))
        1.toShort
      else
        0.toShort
    }
    val cloudLayerMaskTile = UShortArrayTile
      .apply(cloudLayerMaskArrayShort.toArray,
             cloudLayerMaskArray.getDimensions.tail.head,
             cloudLayerMaskArray.getDimensions.head)
      .convert(DoubleCellType)

    val modifiedTiff = originalTiff.mapTile(t => {
      t.mapBands((i, band) => {
        band
          .convert(DoubleCellType)
          .combineDouble(cloudLayerMaskTile)((b, m) => {
            if (m > 0.0)
              0.0
            else
              b
          })
          .combineDouble(shadowLayerMaskTile)((b, m) => {
            if (m > 0.0)
              0.0
            else
              b
          })
          .convert(UShortCellType)
      })
    })

    endTime = System.currentTimeMillis()
    println(s"applied masking in  ${endTime - stTime} msecs")

    (shadowLayerMaskTile, cloudLayerMaskTile, modifiedTiff)
  }

  def validate(originalTiff: MultibandGeoTiff,
               shadowLayerMaskTile: Tile,
               cloudLayerMaskTile: Tile,
               modifiedTiff: MultibandGeoTiff): Unit = {
    //validation
    val shadow = shadowLayerMaskTile.toArrayDouble().count(_ > 0.0)
    val cloud = cloudLayerMaskTile.toArrayDouble().count(_ > 0.0)

    val original = for { b <- originalTiff.tile.bands } yield {
      b.toArray().count(_ > 0)
    }

    val modified = for { b <- modifiedTiff.tile.bands } yield {
      (b.toArray().count(_ > 0), b.toArray().count(_ > 0) + shadow + cloud)
    }

    println(
      s"shadow: $shadow, cloud: $cloud, original: $original, modified: $modified")

  }

  def process(tiffFile: String)
             (implicit jep: Jep): (Tile, Tile, MultibandGeoTiff) = {

    var stTime = System.currentTimeMillis()
    var endTime = System.currentTimeMillis()

    val originalTiff = readTiff(tiffFile)

    endTime = System.currentTimeMillis()
    println(s"read the tiff $tiffFile in ${endTime - stTime} msecs")
    stTime = System.currentTimeMillis()

    val (shadowLayerMaskTile, cloudLayerMaskTile, modifiedTiff) =
      process("PlanetScope", originalTiff)

    validate(originalTiff,
             shadowLayerMaskTile,
             cloudLayerMaskTile,
             modifiedTiff)
    endTime = System.currentTimeMillis()
    println(s"total time to process $tiffFile is ${endTime - stTime} msecs")
    (shadowLayerMaskTile, cloudLayerMaskTile, modifiedTiff)
  }

  def main(args: Array[String]): Unit = {

    val scriptFileName =
      "/Users/sridharalla/git/griffin2/griffin/cloudMasking/MLP_cloudMasking/segmentationBasedCloudMask.py"
    implicit val jep = new Jep()
    jep.runScript(scriptFileName)

    jep.set("__file__", scriptFileName)
    jep.eval("import numpy")



    process("data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_Cloudy_image.tif")
    process("data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_Cloudy2.tif")
    process("data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_cloudy3.tif")


  }

}
