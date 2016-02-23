#' HCL Color Specification
#'
#' Create a vector of colors from hue, chromacity, and lightness.
#'
#' @template hue
#' @param c chromacity, numeric vector with values in \code{[0, ~1]}; 0 is grey, ~1 is full color.
#' @template lightness
#'
#' @template color_spec
#' @template color_spec_from_matrix
#'
#' @details
#' In HCL space, the perceived color (hue) is completely separated from the perceived intensity (chromacity) and lightness of the color. This means that colors of various hues but same chromacity and lightness appear as the exact same grey when converted to black and white. This makes the HCL space particularly suitable to create good color palettes:
#' \itemize{
#'   \item For qualitative palettes (discrete variables): varying h at constant c and l avoids drawing attention to certain hues as it would happen if the same was done in HSV or HSL space. Indeed, some hues are perceived as brighter (yellow, light green, etc.), others as duller/darker (blues, etc.).
#'   \item For sequential palettes (continuous variables): varying l (or possibly c) for a constant h gives a sense of direction and avoid the many perceptual pitfalls typical of 'rainbow'-like scales.
#' }
#'
#' @examples
#' hcl()
#' hcl(330, 1, 0.6)
#' hcl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hcl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hcl(c(330, 340), 1, 0.6)
#'
#' # compare color palettes
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hcl(h=ramp*360), hsv(h=ramp*360, s=0.9), hsl(h=ramp*360, s=0.9))
#' show_col(hcl(c=ramp))
#' show_col(hcl(l=ramp), rainbow(10), heat.colors(10))
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
lch <- function(l=0.6, c=0.6, h=0) {
  # handle color channels
  x <- tabularise_arguments(l, c, h)

  # parse colors using chroma.js
  colors <- parse_color(x, "lch")
    
  return(colors)
}
