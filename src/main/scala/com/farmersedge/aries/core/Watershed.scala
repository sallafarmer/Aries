package com.farmersedge.aries.core

import com.farmersedge.aries.core.ColorConversions._

object Watershed {

  /*
  def _mask_filter_result(result, mask):
    """Return result after masking.

    Input masks are eroded so that mask areas in the original image don't
    affect values in the result.
    """
    if mask is None:
        result[0, :] = 0
        result[-1, :] = 0
        result[:, 0] = 0
        result[:, -1] = 0
        return result
    else:
        mask = binary_erosion(mask, EROSION_SELEM, border_value=0)
        return result * mask

    image = img_as_float(image)
    result = convolve(image, HSOBEL_WEIGHTS)
    return _mask_filter_result(result, mask)

   */
  def sobel(grays: Array[GRAY]): Array[Array[Int]] = {
    //  out = np.sqrt(sobel_h(image, mask)**2 + sobel_v(image, mask)**2)
    //  out /= np.sqrt(2)
    null
  }

  def run(rgbs: Array[RGB]): Array[Array[Int]] = {

    //implement watershed algorithm now
    val gradient = sobel(rgb2gray(rgbs))
//    segments = watershed(gradient, markers = self.numOfSuperpixels, compactness = 0.001)
    null
  }

}
