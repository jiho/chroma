#' HSV Color Specification
#'
#' Create a vector of colors from hue, saturation, and value.
#'
#' @template param_hue
#' @template param_saturation
#' @param v value, number in \code{[0, 1]}; 0 is black, 1 is full brightness
#' @template param_alpha
#' @param max.hue maximum value for the hue component. Set to 1 to make this function a drop-in replacement for the \code{\link[grDevices]{hsv}} function, included with R in package \code{grDevices}.
#'
#' @template color_spec_from_matrix
#' @details
#' The HSV color model tentatively separates color (hue), color intensity (saturation), and color lightness (value), which helps with the creation of color palettes compared to RGB. However, while the color components are separated numerically, some confusion remains in the way colors are perceived by the human eye/brain. Indeed, even at constant \code{s} and \code{v} some hues are perceived as brighter (yellow and green for example) and therefore draw the viewer's attention. This is one of the common problems with 'rainbow'-like color scales (which are constructed in HSL or HSV space). See the function \code{\link{luminance}} for a numerical demonstration. For the creation of color palettes, the HCL space (function \code{\link{hcl}}) should be preferred.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @export
#'
#' @examples
#' hsv()
#' hsv(330, 1, 0.6)
#' hsv(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsv(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsv(c(330, 340), 1, 0.6)
#'
#' # Color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hsv(h=ramp*360), hsv(s=ramp), hsv(v=ramp))
#'
#' # Recreate the rainbow() scale
#' show_col(hsv(h=seq(0, 324, length.out=10), s=1, v=1), rainbow(10))
hsv <- function(h=0, s=0.6, v=0.7, alpha=NULL, max.hue=360) {
  # handle color channels
  x <- tabularise_arguments(h, s, v)

  # convert hue
  if (max.hue != 360) {
    x[,1] <- x[,1] / max.hue * 360
  }

  # parse colors using chroma.js
  colors <- parse_color(x, "hsv")

  # add transparency if needed
  if ( !is.null(alpha) ) {
    colors <- alpha(colors, alpha)
  }

  return(colors)
}
