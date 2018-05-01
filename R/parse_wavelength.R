#' Correspondence table between wavelengths of visible light and sRGB colors
#'
#' @format A data.frame with 77 rows and 2 variables:
#' \describe{
#'   \item{wl}{the wavelength, in nm.}
#'   \item{color}{the hex code of the correpsonding color.}
#' }
#' @details As explained by the author, David Eccles: "Colour ranges were taken from 'CRC Handbook of Fundamental Spectroscopic Correlation Charts' (see \url{https://en.wikipedia.org/wiki/Visible_spectrum}). Spectral XYZ locus coordinates were found at \url{http://www.cie.co.at/main/freepubs.html}. Every 10nm, the spectral locus point was used as the initial point in a search for a matching sRGB colour (assuming "Dim" viewing condition). The search was carried out by a binary search along a line in JCh space between the spectral point, and a point with 0.3 * brightness (J), 0 chroma (C), and the same hue (h) as the spectral point. The search finished when a point was found that was in sRGB space. The CAT02 matrix was modified as suggested in Brill(2008) \url{https://doi.org/10.1002/col.20432} to reduce yellow-blue problems by replacing the bottom row (0.0030, 0.0136, 0.9834) with (0,0,1). An iterative smoothing was carried out to eliminate the largest jumps in chroma (greater than 5\% change). Code used to generate the colour gradient can be found at \url{http://www.gringene.org/code/spectrum.r}."
#' @source \url{https://en.wikipedia.org/wiki/Visible_spectrum#/media/File:Linear_visible_spectrum.svg}
#' @seealso \code{\link{wavelength}} which uses the table to convert any wavelength value to an R color.
"visible"

#' Color Specification as Wavelength
#'
#' Convert colors specified as the wavelength of a beam of monochromatic light into an R color.
#'
#' @param x vector of wavelengths.
#'
#' @details The human eye can distinguish colors between approximately 380 and 750 nm. The colors that we name (arbitrarily, since they really change continuously along the spectrum) are: Violet = [380,450] nm, Blue = [450,495] nm, Green = [495,570] nm, Yellow = [570,590] nm, Orange = [590,620] nm, Red = [620,750] nm.
#'
#' The correspondance with colors in sRGB-space (the ones R can use) is only approximate because not all monochromatic light colors can be accurately represented in sRGB. Technically it is done by interpolating from a lookup table, described in \code{\link{visible}}, which itself is a numerical approximation of the match between input colours and sRGB equivalents.
#'
#' @template return_hex_colors
#'
#' @family color specifications
#'
#' @export
#' @importFrom stats splinefun
#' @examples
#' # Display the full visible spectrum
#' x <- seq(370, 760, length.out=200)
#' plot(x=x, y=rep(0, 200), col=wavelength(x), pch="|", cex=5)
#' abline(v=c(380, 450, 495, 570, 590, 620, 750), lty="dotted")
wavelength <- function(x) {
  parse_color(x, "wavelength")
}

#' @rdname wavelength
#' @export
wl <- wavelength

# Internal implementation of wavelength parsing
# @param x vector of wavelengths.
parse_wavelength <- function(x) {
  # interpolate from the values in the table
  interp_scale(chroma::visible$color, values=chroma::visible$wl, extrapolate=TRUE, na.value=NA)(x)
}

# Internal implementation of the conversion into wavelength
# @param x vector of input colors specified as hex codes.
convert_wavelength <- function(x) {
  # get the table of visible spectrum colors
  v <- chroma::visible
  # and subsample it to speed up the function
  v <- v[seq(1, nrow(v), by=2),]

  # compute the distance between input colors and each color in the table
  dists <- lapply(x, deltaE, v$color)

  # for each input color
  res <- sapply(dists, function(y) {
    if (all(is.na(y))) {
      min_wl <- NA_real_
    } else {
      # find the closest color in the visible spectrum table
      i <- which.min(y)
      # look around it to find the acutal closest wavelength
      is <- max(1, i-2):min(length(v$wl), i+2)
      # NB: we use spline approximation because the deltaE distance is not linear and we are looking for the minimum which is likely *between* known points so we have to interpolate the dip in between those.
      approxfun <- stats::splinefun(v$wl[is], y[is])
      wl_local <- seq(min(v$wl[is]), max(v$wl[is]), by=0.5)
      min_wl <- wl_local[which.min(approxfun(wl_local))]
    }
    return(min_wl)
  })

  return(res)
}
