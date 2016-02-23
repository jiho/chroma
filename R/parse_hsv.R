#' HSV Color Specification
#'
#' Create a vector of colors from hue, saturation, and value.
#'
#' @template hue
#' @template saturation
#' @param v value, same conventions as \code{s}; 0 is black, 1 is full brightness
#'
#' @template color_spec
#' @template color_spec_from_matrix
#'
#' @examples
#' hsv()
#' hsv(330, 1, 0.6)
#' hsv(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsv(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsv(c(330, 340), 1, 0.6)
#'
#' # color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hsv(h=ramp*360))
#' show_col(hsv(s=ramp))
#' show_col(hsv(v=ramp))
#'
#' @export
# TODO make compatible with grDevices::hcl
hsv <- function(h=0, s=0.6, v=0.7) {
  # handle color channels
  x <- tabularise_arguments(h, s, v)

  # parse colors using chroma.js
  colors <- parse_color(x, "hsv")
  
  return(colors)
}
