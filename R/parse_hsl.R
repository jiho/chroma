#' HSL Color Specification
#'
#' Create a vector of colors from hue, saturation, and lightness.
#'
#' @template param_hue
#' @template param_saturation
#' @template param_lightness
#'
#' @template color_spec_from_matrix
#' @details
#' The HSL color model tentatively separates color (hue), color intensity (saturation), and color lightness, which helps with the creation of color palettes compared to RGB. However, while the color components are separated numerically, some confusion reamins in the way colors are perceived by the human eye/brain. Indeed, even at constant \code{s} and \code{l} some hues are perceived as brighter (yellow and green for example) and therefore draw attention to themselves. This is one of the common problems with 'rainbow'-like color scales (which are constructed in HSL or HSV space). See the function \code{\link{luminance}} for a numerical demonstration. For the creation of color palettes, the HCL space (function \code{\link{hcl}}) should be preferred.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @examples
#' hsl()
#' hsl(330, 1, 0.6)
#' hsl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsl(c(330, 340), 1, 0.6)
#'
#' # Color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hsl(h=ramp*360), hsl(s=ramp), hsl(l=ramp))
#'
#' @export
hsl <- function(h=0, s=0.5, l=0.5) {
  # handle color channels
  x <- tabularise_arguments(h, s, l)

  # parse colors using chroma.js
  colors <- parse_color(x, "hsl")
  
  return(colors)
}
