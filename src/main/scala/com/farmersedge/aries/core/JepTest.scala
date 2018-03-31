package com.farmersedge.aries.core

import java.io.InputStreamReader
import java.nio.{ByteBuffer, DoubleBuffer, FloatBuffer, IntBuffer}

import com.farmersedge.aries.core.ZoneProcessor.{bandsToIndexes, readTiff}
import geotrellis.raster.{
  ArrayTile,
  DoubleCellType,
  Tile,
  UShortArrayTile,
  UShortCellType
}
import jep.{DirectNDArray, Jep, NDArray}

import scala.collection.mutable

object JepTest {

  def getDirectNDArray(band: Tile): DirectNDArray[DoubleBuffer] = {
    val rows = band.rows
    val cols = band.cols

    val band_buffer: DoubleBuffer =
      ByteBuffer.allocateDirect(rows * cols * 8).asDoubleBuffer()
    band_buffer.put(band.toArrayDouble(), 0, rows * cols)
    val band_data = new DirectNDArray[DoubleBuffer](band_buffer)
    band_data
  }

  def main(args: Array[String]): Unit = {
    var stTime = System.currentTimeMillis()
    var endTime = System.currentTimeMillis()

    val tiffFile =
      "data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_Cloudy_image.tif"
    val original = readTiff(tiffFile)
    val red_band = original.raster.tile.band(bandsToIndexes('r))
    val green_band = original.raster.tile.band(bandsToIndexes('g))
    val blue_band = original.raster.tile.band(bandsToIndexes('b))
    val nir_band = original.raster.tile.band(bandsToIndexes('nir))

    val rows = red_band.rows
    val cols = red_band.cols

    val red_band_data = getDirectNDArray(red_band)
    val green_band_data = getDirectNDArray(green_band)
    val blue_band_data = getDirectNDArray(blue_band)
    val nir_band_data = getDirectNDArray(nir_band)

    endTime = System.currentTimeMillis()
    println(s"time taken ${endTime - stTime} msecs")
    stTime =  System.currentTimeMillis()

    val scriptFileName =
      "/Users/sridharalla/git/griffin2/griffin/cloudMasking/MLP_cloudMasking/segmentationBasedCloudMask.py"
    val jep = new Jep()

    jep.runScript(scriptFileName)

    jep.set("__file__", scriptFileName)
    jep.eval("import numpy")

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

    jep.eval(
      "source = {'AcquisitionDate': datetime.datetime(2017,8,2),'service': 'PlanetScope'}")

    jep.eval("cloudMaskGenerator = SegmentationBasedCloudMask()")

    jep.eval("cloudMask = cloudMaskGenerator.generateMask(bands, source)")

    val cloudMask = jep.getValue("cloudMask")
    val shadowLayer = jep.getValue("cloudMask.shadowLayer")
    val cloudLayer = jep.getValue("cloudMask.cloudLayer")

    endTime = System.currentTimeMillis()
    println(s"time taken ${endTime - stTime} msecs")
    stTime =  System.currentTimeMillis()

    val shadowLayerMask = shadowLayer.asInstanceOf[NDArray[Array[Boolean]]]
    val cloudLayerMask = cloudLayer.asInstanceOf[NDArray[Array[Boolean]]]

    val mask = for {
      i <- cloudLayerMask.getData.indices
    } yield {
      if (shadowLayerMask.getData()(i) || cloudLayerMask.getData()(i))
        1.toShort
      else
        0.toShort
    }
    val maskTile = UShortArrayTile
      .apply(mask.toArray,
             shadowLayerMask.getDimensions.tail.head,
             shadowLayerMask.getDimensions.head)
      .convert(DoubleCellType)

    val modifiedTile = original.mapTile(t => {
      t.mapBands((i, band) => {
        band
          .convert(DoubleCellType)
          .combineDouble(maskTile)((b, m) => {
            if (m >0.0)
              0.0
            else
              b
          })
          .convert(UShortCellType)
      })
    })

/*    val x = shadowLayerMask.getData().count(_ == true)
    val y = cloudLayerMask.getData().count(_ == true)
    val z = mask.count(_ > 0)
*/
 /*   val v = for { b <- original.tile.bands } yield { b.toArray().filter(_ > 0) }

    val v2 = for { b <- modifiedTile.tile.bands } yield {
      b.toArray().filter(_ > 0)
    }
*/
    endTime = System.currentTimeMillis()
    println(s"time taken ${endTime - stTime} msecs")
  }

}
