#' HSI color specification
#'
#' Create a vector of colors from hue, saturation, and intensity.
#'
#' @template param_hue
#' @template param_saturation
#' @param i intensity, number in \code{[0,2]}; 0 is black, 2 is white.
#'
#' @template color_spec_from_matrix
#' @details
#' The HSI color model tentatively separates color (hue), color intensity (saturation), and color intensity, which helps with the creation of color palettes compared to RGB. However, while the color components are separated numerically, some confusion remains in the way colors are perceived by the human eye/brain. Indeed, even at constant \code{s} and \code{i} some hues are perceived as brighter (yellow and green for example) and therefore draw the viewer's attention. This is one of the common problems with 'rainbow'-like color scales (which are constructed in HSL or HSV space). See the function \code{\link{luminance}} for a numerical demonstration. For the creation of color palettes, the HCL space (function \code{\link{hcl}}) should be preferred.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @export
#'
#' @examples
#' hsi()
#' hsi(330, 1, 0.6)
#' hsi(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsi(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsi(c(330, 340), 1, 0.6)
#'
#' # Color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hsi(h=ramp*360), hsi(s=ramp), hsi(i=ramp*2))
hsi <- function(h=0, s=0.5, i=0.5) {
  # handle color channels
  x <- tabularise_arguments(h, s, i)

  # parse colors using chroma.js
  colors <- parse_color(x, "hsi")

  return(colors)
}
