#' HCL Color Specification
#'
#' Create a vector of colors from hue, chroma, and lightness.
#'
#' @template hue
#' @param c chroma, numeric vector with values in \code{[0, 1]}; 0 is grey, 1 is full color.
#' @template lightness
#'
#' @template color_spec
#' @template color_spec_from_matrix
#'
# TODO: add advice such as the following in all functions
# In HCL space, the perceived color (hue) is completely separated from the perceived lightness of the color. All colors on a pure hue scale (same \code{c} and \code{l}) have the same apparent brightness.
#'
#' @examples
#' hcl()
#' hsl(330, 1, 0.6)
#' hsl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsl(c(330, 340), 1, 0.6)
#'
#' # color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hcl(h=ramp*360))
#' show_col(hcl(c=ramp))
#' show_col(hcl(l=ramp))
#'
#' @export
# TODO make compatible with grDevices::hcl
hcl <- function(h=0, c=0.6, l=0.6) {
  # handle color channels
  x <- tabularise_arguments(h, c, l)

  # parse colors using chroma.js
  colors <- parse_color(x, "hcl")
    
  return(colors)
}

#' @name hcl
#' @export
lch <- function(l=0.5, c=0.5, h=0) {
  hcl(h, c, l)
}
