package com.farmersedge.aries.core

object Clahe {

  val NR_OF_GREY = Math.pow(2, 14) // number of grayscale levels to use in CLAHE algorithm

  def img_as_uint(in: Array[Array[Double]]): Array[Array[Int]] = {
    in.map(_.map(x => (x * 65535.0).toInt))
  }

  def img_as_double(in: Array[Array[Int]]): Array[Array[Double]] = {
    in.map(_.map(x => (x * 65535.0).toDouble))
  }

  def rescale_intensity(vgrid: Array[Array[Double]],
                        omax: Double): Array[Array[Double]] = {
    val imin = vgrid.flatten.min.toDouble
    val imax = vgrid.flatten.max.toDouble

    val omin = 0.0

    val vgridOut = vgrid.map(
      _.map(v => ((v.toDouble - imin) / (imax - imin)) * (omax - omin) + omin))

    vgridOut
  }

  def equalize_adapthist(vgridIn: Array[Array[Double]],
                         shape: (Int, Int),
                         clip_limit: Double): Array[Array[Double]] = {

    val nbins = 256

    //    val vgrid1 = img_as_uint(vgridIn)

    val vgrid2 = rescale_intensity(vgridIn, NR_OF_GREY - 1)

    val kernel_size = (shape._1 / 8, shape._2 / 8)

    val vgrid3 = runClahe(vgrid2, shape, kernel_size, clip_limit * nbins, nbins)

    //   val vgrid3 = img_as_double(vgrid2)
    val vgrid4 = rescale_intensity(vgrid3, 65535.0)

    vgrid4
  }

  def map_histogram(hist: Array[Int], n_pixels: Int): Array[Int] = {
    val min_val = 0
    val max_val = NR_OF_GREY - 1

    val scale = (max_val - min_val).toDouble / n_pixels.toDouble
    val cmHist = hist.map {
      var s = 0
      d: Int =>
        {
          s += d
          var out = (s.toDouble * scale) + min_val
          if (out > max_val) out = max_val
          out.toInt
        }
    }

    cmHist
  }

  def runClahe(image: Array[Array[Double]],
               shape: (Int, Int),
               kernel_size: (Int, Int),
               clip_limit: Double,
               nbins: Int): Array[Array[Double]] = {

    val nr: Int = (Math.ceil(shape._1 / kernel_size._1)).toInt
    val nc: Int = (Math.ceil(shape._2 / kernel_size._2)).toInt

    val row_step: Int = (Math.floor(shape._1 / nr)).toInt
    val col_step: Int = (Math.floor(shape._2 / nc)).toInt

    val bin_size = 1 + NR_OF_GREY / nbins
    val lut = (0 until NR_OF_GREY.toInt) //.map(_/bin_size)

    val map_array = Array.ofDim[Int](nr, nc, nbins)

    for {
      r <- 0 until nr
      c <- 0 until nc
    } yield {
      val subImage = image
        .map(_.slice(c * col_step, (c + 1) * col_step))
        .slice(r * row_step, (r + 1) * row_step)

      val clim =
        if (clip_limit > 0.0) // Calculate actual cliplimit
          (clip_limit * col_step * row_step / nbins).toInt
        else NR_OF_GREY

      val emptyHistogram = (0 until nbins).map(_ -> 0).toMap

      var hist = subImage.flatten
        .map(x => (x / bin_size).toInt)
        .foldLeft(emptyHistogram) { (m, c) =>
          m + (c -> (m.getOrElse(c, 0) + 1))
        }

      val sortedHist = hist.toSeq.sortBy(_._1).map(_._2).toArray

      val cmHist = map_histogram(sortedHist, row_step * col_step)

      map_array(r)(c) = cmHist

    }

    // Interpolate greylevel mappings to get CLAHE image
    var rstart = 0.0
    var r_offset = 0.0
    var c_offset = 0.0
    var rU = 0
    var rB = 0
    var cL = 0
    var cR = 0

    for (r <- 0 to nr) {
      var cstart = 0.0
      if (r == 0) {
        //special case: top row
        r_offset = row_step / 2.0
        rU = 0
        rB = 0
      } else if (r == nr) {
        //#special case: bottom row
        r_offset = row_step / 2.0
        rU = nr - 1
        rB = rU
      } else {
        //#default values
        r_offset = row_step
        rU = r - 1
        rB = rB + 1
      }

      for (c <- 0 to nc) {
        if (c == 0) {
          //#special case: left column
          c_offset = col_step / 2.0
          cL = 0
          cR = 0
        } else if (c == nc) {
          //#special case: right column
          c_offset = col_step / 2.0
          cL = nc - 1
          cR = cL
        } else { //#default values
          c_offset = col_step
          cL = c - 1
          cR = cL + 1
        }

        val mapLU = map_array(rU)(cL)
        val mapRU = map_array(rU)(cR)
        val mapLB = map_array(rB)(cL)
        val mapRB = map_array(rB)(cR)

        val cslice = (cstart.toInt until cstart.toInt + c_offset.toInt).toArray
        val rslice = (rstart.toInt until rstart.toInt + r_offset.toInt).toArray

        interpolate(image, cslice, rslice, mapLU, mapRU, mapLB, mapRB, lut.toArray)

        cstart += c_offset
      }
      rstart += r_offset
    }

    image
  }

  def interpolate(image: Array[Array[Double]],
                  xslice: Array[Int],
                  yslice: Array[Int],
                  mapLU: Array[Int],
                  mapRU: Array[Int],
                  mapLB: Array[Int],
                  mapRB: Array[Int],
                  lut: Array[Double]): Array[Array[Double]] = {

    return image
  }

  def run(hsvs: Array[HSV], shape: (Int, Int)): Array[RGB] = {

    //copy v from hsv into 2 dimensional array
    val vgrid = Array.ofDim[Double](shape._1, shape._2)

    for {
      r <- 0 until shape._1
      c <- 0 until shape._2
    } yield {
      val k = r * shape._1 + c
      vgrid(r)(c) = hsvs(k).v
    }

    //  equalize_adapthist on the vgrid
    equalize_adapthist(vgrid, shape, 0.04)

    //modify hsvs
    ColorConversions.hsv2rgb(hsvs)
  }

  /*
  def equalize_adapthist(image, kernel_size=None,
                       clip_limit=0.01, nbins=256, **kwargs):
    """Contrast Limited Adaptive Histogram Equalization (CLAHE).

    An algorithm for local contrast enhancement, that uses histograms computed
    over different tile regions of the image. Local details can therefore be
    enhanced even in regions that are darker or lighter than most of the image.

    Parameters
    ----------
    image : (M, N[, C]) ndarray
        Input image.
    kernel_size: integer or list-like, optional
        Defines the shape of contextual regions used in the algorithm. If
        iterable is passed, it must have the same number of elements as
        ``image.ndim`` (without color channel). If integer, it is broadcasted
        to each `image` dimension. By default, ``kernel_size`` is 1/8 of
        ``image`` height by 1/8 of its width.
    clip_limit : float, optional
        Clipping limit, normalized between 0 and 1 (higher values give more
        contrast).
    nbins : int, optional
        Number of gray bins for histogram ("data range").

    Returns
    -------
    out : (M, N[, C]) ndarray
        Equalized image.

    See Also
    --------
    equalize_hist, rescale_intensity

    Notes
    -----
 * For color images, the following steps are performed:
       - The image is converted to HSV color space
       - The CLAHE algorithm is run on the V (Value) channel
       - The image is converted back to RGB space and returned
 * For RGBA images, the original alpha channel is removed.

    References
    ----------
    .. [1] http://tog.acm.org/resources/GraphicsGems/
    .. [2] https://en.wikipedia.org/wiki/CLAHE#CLAHE
    """
    image = img_as_uint(image)
    image = rescale_intensity(image, out_range=(0, NR_OF_GREY - 1))

    if kwargs:
        if 'ntiles_x' in kwargs or 'ntiles_y' in kwargs:
            msg = '`ntiles_*` have been deprecated in favor of `kernel_size`'
            raise ValueError(msg)

    if kernel_size is None:
        kernel_size = (image.shape[0] // 8, image.shape[1] // 8)
    elif isinstance(kernel_size, numbers.Number):
        kernel_size = (kernel_size,) * image.ndim
    elif len(kernel_size) != image.ndim:
        ValueError('Incorrect value of `kernel_size`: {}'.format(kernel_size))

    kernel_size = [int(k) for k in kernel_size]

    image = _clahe(image, kernel_size, clip_limit * nbins, nbins)
    image = img_as_float(image)
    return rescale_intensity(image)

 */

}
