#' HSL Color Specification
#'
#' Create a vector of colors from hue, saturation, and lightness.
#'
#' @template hue
#' @template saturation
#' @template lightness
#'
#' @template color_spec
#' @template color_spec_from_matrix
#'
#' @examples
#' hsl()
#' hsl(330, 1, 0.6)
#' hsl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsl(c(330, 340), 1, 0.6)
#'
#' # color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hsl(h=ramp*360))
#' show_col(hsl(s=ramp))
#' show_col(hsl(l=ramp))
#'
#' @export
hsl <- function(h=0, s=0.5, l=0.5) {
  # handle color channels
  x <- tabularise_arguments(h, s, l)

  # parse colors using chroma.js
  colors <- parse_color(x, "hsl")
  
  return(colors)
}
