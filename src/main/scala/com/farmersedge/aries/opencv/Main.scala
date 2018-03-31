package com.farmersedge.aries.opencv

import org.opencv.core.Core

object Main {

//  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

  def main(args: Array[String]): Unit = {

    var zoneMapFile = s"data/(controller)_2017-08-30-2017-09-05__274501__Harvest/2017-08-30-2017-09-05__274502__Harvest__ZoneMap.tiff"
    var tiffFile = s"data/20170217_215326_0c22_1B_AnalyticMS_274501.tiff"

//    ZoneProcessor.generateNDVIPerZone(tiffFile, zoneMapFile)
    CloudMasking_MLP.genCloudMask(tiffFile)


  }
}
