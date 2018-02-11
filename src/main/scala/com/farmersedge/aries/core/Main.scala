package com.farmersedge.aries.core

import geotrellis.raster.io.geotiff.SinglebandGeoTiff
import geotrellis.raster.io.geotiff.reader.GeoTiffReader

object Main {

/*
    ImageProcessor.process(s"data/(controller)_2017-08-30-2017-09-05__274501__Harvest/2017-08-30-2017-09-05__274502__Harvest__ZoneMap.tiff")
    ImageProcessor.process(s"data/20170217_215326_0c22_1B_AnalyticMS_274501.tiff")

    ImageProcessor.process(s"data/(controller)_2017-09-13-2017-09-28__297113__Harvest/2017-09-13-2017-09-28__297114__Harvest__ZoneMap.tiff")
    ImageProcessor.process(s"data/20170223_165518_0e30_1B_AnalyticMS_297113.tiff")


 */
  def main(args: Array[String]): Unit = {

    var zoneMapFile = s"data/(controller)_2017-08-30-2017-09-05__274501__Harvest/2017-08-30-2017-09-05__274502__Harvest__ZoneMap.tiff"
    var tiffFile = s"data/20170217_215326_0c22_1B_AnalyticMS_274501.tiff"

    ZoneProcessor.generateNDVIPerZone(tiffFile, zoneMapFile)

    zoneMapFile = s"data/(controller)_2017-09-13-2017-09-28__297113__Harvest/2017-09-13-2017-09-28__297114__Harvest__ZoneMap.tiff"
    tiffFile = s"data/20170223_165518_0e30_1B_AnalyticMS_297113.tiff"


    ZoneProcessor.generateNDVIPerZone(tiffFile, zoneMapFile)
  }
}
