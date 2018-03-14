package com.farmersedge.aries.core

object Clahe {

  val NR_OF_GREY = Math.pow(2, 14) // number of grayscale levels to use in CLAHE algorithm

  def img_as_uint(in: Array[Array[Double]]): Array[Array[Double]] = {
    in.map(_.map(x => (x * 65535.0).toDouble))
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

    val vgrid1 = img_as_uint(vgridIn)

    val vgrid2 = rescale_intensity(vgrid1, NR_OF_GREY - 1)

    val kernel_size = (shape._1 / 8, shape._2 / 8)

    val vgrid3 = runClahe(vgrid2, shape, kernel_size, clip_limit * nbins, nbins)

    val vgrid4 = vgrid3.map(_.map(x => (x /65535.0).toDouble))

    //   val vgrid3 = img_as_double(vgrid2)
    val vgrid5 = rescale_intensity(vgrid4, 1.0)

    vgrid5
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

  def clip_histogram(hist: Array[Int], clip_limit: Double): Array[Int] = {
    val excess_mask = for {
      h <- hist
    } yield {
      if (h > clip_limit) true else false
    }

    val excess_mask_hist = hist.zip(excess_mask).collect{case (h, true) => h}

    val excess_sum = excess_mask_hist.sum

    var n_excess = excess_sum - excess_mask_hist.size * clip_limit


    // Second part: clip histogram and redistribute excess pixels in each bin
    val bin_incr = n_excess.toInt/hist.size  //average binincrement
    val upper = clip_limit - bin_incr  //Bins larger than upper set to cliplimit

    val low_mask = for {
      h <- hist
    } yield {
      if (h < upper) true else false
    }
    val low_mask_hist = hist.zip(low_mask).collect{case (h, true) => h}

    val mid_mask = for {
      h <- hist
    } yield {
      if (h >= upper && h < clip_limit) true else false
    }

    val mid_mask_hist = hist.zip(mid_mask).collect{case (h, true) => h}

    var clippedHist = for {
      h <- hist

    } yield {
      if (h >= upper) clip_limit.toInt else h+bin_incr
    }


    n_excess = n_excess - low_mask_hist.size * bin_incr
    n_excess = n_excess - mid_mask_hist.size * clip_limit - mid_mask_hist.sum

    var prev_n_excess = n_excess

    var breakloop = false
    while (n_excess > 0 && !breakloop) {
      //# Redistribute remaining excess
      var index = 0
      while (n_excess > 0 && index < hist.size) {

        val clip_mask = for {
          h <- clippedHist
        } yield {
          if (h < clip_limit) true else false
        }
        val clip_mask_hist = clippedHist.zip(clip_mask).collect{case (h, true) => h}

        var step_size = (clip_mask_hist.size / n_excess.toInt)
        step_size = Math.max(step_size, 1)

        var under_mask = for {
          h <- clippedHist
        } yield {
          if (h < 0) true else false
        }
        for(i <- index until clippedHist.size by step_size) {
          under_mask(i) = true
        }

        under_mask = under_mask.zip(clip_mask).collect{case (l,r) => l && r}

        val under_mask_hist = clippedHist.zip(under_mask).collect{case (h, true) => h}
        clippedHist = clippedHist.zip(under_mask).collect{case (h, u) => if (u) h+1 else h}

        n_excess = n_excess - under_mask_hist.sum
        index += 1
      }

      if (prev_n_excess == n_excess)
        breakloop = true
      prev_n_excess = n_excess
    }


    clippedHist
  }

  def runClahe(image: Array[Array[Double]],
               shape: (Int, Int),
               kernel_size: (Int, Int),
               clip_limit: Double,
               nbins: Int): Array[Array[Double]] = {

    val nr: Int = (Math.ceil(shape._1.toDouble / kernel_size._1.toDouble)).toInt
    val nc: Int = (Math.ceil(shape._2.toDouble / kernel_size._2.toDouble)).toInt

    val row_step: Int = (Math.floor(shape._1 / nr)).toInt
    val col_step: Int = (Math.floor(shape._2 / nc)).toInt

    val bin_size = 1 + NR_OF_GREY / nbins
    val lut = (0 until NR_OF_GREY.toInt).map(_/bin_size).map(_.toInt)

    val map_array = Array.ofDim[Int](nr, nc, nbins)

    for {
      r <- 0 until nr
      c <- 0 until nc
    } yield {
      val subImage = image
        .map(_.slice(c * col_step, (c + 1) * col_step))
        .slice(r * row_step, (r + 1) * row_step)

      val s = clip_limit * col_step.toDouble * row_step.toDouble
      val clim =
        if (clip_limit > 0.0) // Calculate actual cliplimit
          Math.ceil(clip_limit * col_step.toDouble * row_step.toDouble / nbins.toDouble).toInt
        else NR_OF_GREY

      val emptyHistogram = (0 until nbins).map(_ -> 0).toMap

      var hist = subImage.flatten
        .map(x => (x / bin_size).toInt)
        .foldLeft(emptyHistogram) { (m, c) =>
          m + (c -> (m.getOrElse(c, 0) + 1))
        }

      val sortedHist = hist.toSeq.sortBy(_._1).map(_._2).toArray

      val clippedHist = clip_histogram(sortedHist, clim)

      val cmHist = map_histogram(clippedHist, row_step * col_step)

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

        val cslice = (cstart until cstart + c_offset by 1d).toArray
        val rslice = (rstart until rstart + r_offset by 1d).toArray

        val newImage = interpolate(image, cslice, rslice, mapLU, mapRU, mapLB, mapRB, lut.toArray)

        //update image with the new interpolated image

        for {
          y <- rslice.indices
          x <- cslice.indices
        } yield {
          image(rslice(y).toInt)(cslice(x).toInt) = newImage(y.toInt)(x.toInt).toInt
        }

        cstart += c_offset
      }
      rstart += r_offset
    }

    image
  }

  def interpolate(image: Array[Array[Double]],
                  xslice: Array[Double],
                  yslice: Array[Double],
                  mapLU: Array[Int],
                  mapRU: Array[Int],
                  mapLB: Array[Int],
                  mapRB: Array[Int],
                  lut: Array[Int]): Array[Array[Double]] = {

    //Find the new grayscale level for a region using bilinear interpolation.
    /*
    Parameters
    ----------
    image : ndarray
        Full image.
    xslice, yslice : array-like
       Indices of the region.
    map* : ndarray
        Mappings of greylevels from histograms.
    lut : ndarray
        Maps grayscale levels in image to histogram levels.

    Returns
    -------
    out : ndarray
        Original image with the subregion replaced.

    Notes
    -----
    This function calculates the new greylevel assignments of pixels within
    a submatrix of the image. This is done by a bilinear interpolation between
    four different mappings in order to eliminate boundary artifacts.
     */

    val norm = xslice.size * yslice.size  //Normalization factor

    val meshGrid = for {
      y <- 0 until yslice.size
      x <- 0 until xslice.size
    } yield {
      (x, y)
    }

    val x_coef = meshGrid.map(_._1).toArray.grouped(xslice.size).toArray
    val y_coef = meshGrid.map(_._2).toArray.grouped(xslice.size).toArray

    val x_inv_coef = x_coef.map(rows => rows.reverse.map(_+1))
    val y_inv_coef = y_coef.reverse.map(rows => rows.map(_+1))

    val view = image
      .map(_.slice(xslice(0).toInt, xslice.last.toInt+1))
      .slice(yslice(0).toInt, yslice.last.toInt+1)

    val im_slice = view.map(_.map(c => (c/65).toInt))

    val newImage = Array.ofDim[Double](yslice.size, xslice.size)

    val x = 0
    val y = 0
    val im_sliceYX = im_slice(y)(x)
    val z1 = y_inv_coef(y)(x)
    val z2 = x_inv_coef(y)(x)
    val z3 = y_coef(y)(x)
    val z4 = x_coef(y)(x)
    val z5 = im_sliceYX
    val z6 = mapLU(im_sliceYX)
    val z7 = mapRU(im_sliceYX)
    val z8 = mapLB(im_sliceYX)
    val z9 = mapRB(im_sliceYX)

    for {
      y <- 0 until yslice.size
      x <- 0 until xslice.size
    } yield {
      val im_sliceYX = im_slice(y)(x)



   newImage(y)(x)  = ((y_inv_coef(y)(x) * (x_inv_coef(y)(x) * mapLU(im_sliceYX)
       + x_coef(y)(x) * mapRU(im_sliceYX))
       + y_coef(y)(x) * (x_inv_coef(y)(x) * mapLB(im_sliceYX)
       + x_coef(y)(x) * mapRB(im_sliceYX)))
       / norm)
    }

    /*
      new = ((y_inv_coef * (x_inv_coef * mapLU[im_slice]
                          + x_coef * mapRU[im_slice])
            + y_coef * (x_inv_coef * mapLB[im_slice]
                        + x_coef * mapRB[im_slice]))
           / norm)
    view[:, :] = new

     */

    return newImage
  }

  def run(hsvs: Array[HSV], shape: (Int, Int)): Array[RGB] = {

    //copy v from hsv into 2 dimensional array
    val vgrid = Array.ofDim[Double](shape._1, shape._2)

    for {
      r <- 0 until shape._1
      c <- 0 until shape._2
    } yield {
      val k = r * shape._2 + c
      vgrid(r)(c) = hsvs(k).v
    }

    //  equalize_adapthist on the vgrid
    val vgridout = equalize_adapthist(vgrid, shape, 0.04)

    //modify hsvs
    var hsvsOut = for {
      r <- 0 until shape._1
      c <- 0 until shape._2
    } yield {
      val k = r * shape._1 + c
      HSV(hsvs(k).h , hsvs(k).s , vgridout(r)(c))
    }


    ColorConversions.hsv2rgb(hsvsOut.toArray)
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
