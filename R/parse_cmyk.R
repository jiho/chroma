#' CMYK color specification
#'
#' Create a vector of colors from cyan, magenta, yellow, and black
#'
#' @param c,m,y,k color components, numeric vectors with values in \code{[0, 1]}.
#'
#' @template color_spec_from_matrix
#' @details
#' In R (and in chroma) colors are represented internally in the sRGB color space. CMYK is the color model used by printers, which use cyan, magenta, yellow and black ink. The CMYK model and its associated color spaces cannot reproduce all sRGB colors, seen on screens; bright and flashy colors in particular. On the other hand, some CMYK colors are not representable in sRGB; in those cases, they will be converted to the nearest sRGB color. It is therefore possible for two different \code{cmyk} specifications to yield the same sRGB hex code.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @export
#'
#' @examples
#' cmyk()
#' cmyk(0, 0, 0, 0.1)
#' cmyk(data.frame(c(1, 0.5), c(1, 1), c(1, 1), c(0.5, 0.5)))
#' cmyk(matrix(c(1, 0.5, 1, 1, 1, 1, 0.5, 0.5), ncol=4))
#' cmyk(c=0.5, m=1, y=1, k=c(0.25, 0.5, 0.75))
#'
#' # Color ramp
#' show_col(cmyk(k=seq(0, 1, length.out=10)))
cmyk <- function(c=0, m=0.6, y=0.6, k=0.4) {
  # handle color channels
  x <- tabularise_arguments(c, m, y, k)

  # parse colors using chroma.js
  colors <- parse_color(x, "cmyk")

  return(colors)
}