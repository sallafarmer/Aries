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
  //  var tiffFile = s"data/20180126_181810_101f_3B_AnalyticMS.tif"
 //   var tiffFile = s"data/20170223_165518_0e30_1B_AnalyticMS_297113.tiff"

//    ZoneProcessor.generateNDVIPerZone(tiffFile, zoneMapFile)

    val startTime = System.currentTimeMillis()

    for (i <- 0 to 2) {
      tiffFile = "data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_Cloudy_image.tif"
      var ret = CloudMasking.genCloudMask(tiffFile)
      println(s"$tiffFile,${ret._1},${ret._2},sec")

      tiffFile = "data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_Cloudy2.tif"
      ret = CloudMasking.genCloudMask(tiffFile)
      println(s"$tiffFile,${ret._1},${ret._2},sec")

      tiffFile = "data/20170609_174033_1041_3B_AnalyticMS_TOA_clip_cloudy3.tif"
      ret = CloudMasking.genCloudMask(tiffFile)
      println(s"$tiffFile,${ret._1},${ret._2},sec")

    }
    val endTime = System.currentTimeMillis()
    println(endTime- startTime)

    zoneMapFile = s"data/(controller)_2017-09-13-2017-09-28__297113__Harvest/2017-09-13-2017-09-28__297114__Harvest__ZoneMap.tiff"
    tiffFile = s"data/20170223_165518_0e30_1B_AnalyticMS_297113.tiff"


//    ZoneProcessor.generateNDVIPerZone(tiffFile, zoneMapFile)

  //  CloudMasking.genCloudMask(tiffFile)
  }
}
