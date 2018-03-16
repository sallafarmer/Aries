package com.farmersedge.aries.core

import geotrellis.raster
import geotrellis.raster.{
  DoubleCellType,
  DoubleConstantNoDataCellType,
  IntCellType,
  MultibandTile,
  Raster,
  Tile,
  isData
}
import geotrellis.raster.io.geotiff.MultibandGeoTiff
import geotrellis.raster.io.geotiff.reader.GeoTiffReader
import geotrellis.raster.mapalgebra.focal.TargetCell.NoData
import geotrellis.raster.mapalgebra.local.Mask

import scala.collection.mutable.ArrayBuffer

object ZoneProcessor {
  val bandsToIndexes: Map[Symbol, Int] =
    Map('b -> 0, 'g -> 1, 'r -> 2, 'nir -> 3)

  def readTiff(path: String): MultibandGeoTiff = {
    val geoTiff: MultibandGeoTiff = GeoTiffReader.readMultiband(path)
    geoTiff
  }

  def genZoneMasks(zone: Tile): Array[Tile] = {

    val uniq = zone.toArray().distinct.filter(_ > 0)

    val zoneArray = for {
      uz <- uniq
    } yield {

      val modZone = zone.map(x => {
        if (x == uz)
          0
        else
          Int.MinValue
      })

      modZone

    }

    uniq.foreach(z => println(z))
    zoneArray

  }

  def genMaskedBand(band: Tile, zoneArray: Array[Tile]): Array[Tile] = {
    val maskedBandArr = for {
      z <- zoneArray
    } yield {
      Mask.apply(band, z, raster.NODATA, raster.NODATA)
    }

    maskedBandArr
  }

  def ndvi(red_band: Tile, nir_band: Tile): Tile = {

    val ndviTile =
      red_band
        .convert(DoubleConstantNoDataCellType)
        .combineDouble(nir_band.convert(DoubleConstantNoDataCellType)) {
          (r: Double, nir: Double) =>
            if (isData(r) && isData(nir))
              ((nir - r) / (nir + r)) * 10000
            else Double.NaN
        }

    ndviTile
  }

  def generateNDVIPerZone(tiffFile: String, zoneMapFile: String): Unit = {
    val zones = readTiff(zoneMapFile)
    val original = readTiff(tiffFile)

    println("Read the tiffs")

    val zoneArray = genZoneMasks(zones.raster.tile.band(0))

    val red_band = original.raster.tile.band(bandsToIndexes('r))
    val red_maskedBandArr = genMaskedBand(red_band, zoneArray)

    val green_band = original.raster.tile.band(bandsToIndexes('g))
    val green_maskedBandArr = genMaskedBand(green_band, zoneArray)

    val blue_band = original.raster.tile.band(bandsToIndexes('b))
    val blue_maskedBandArr = genMaskedBand(blue_band, zoneArray)

    val nir_band = original.raster.tile.band(bandsToIndexes('nir))
    val nir_maskedBandArr = genMaskedBand(nir_band, zoneArray)

    val ndvis = for {
      (r, n) <- red_maskedBandArr zip nir_maskedBandArr
    } yield {
      ndvi(r, n)
    }

    println("masked")
  }
}
