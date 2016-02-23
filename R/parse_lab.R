#' CIE L* a* b* Color Specification
#'
#' Create a vector of colors from lightness and a and b color-opponents.
#'
#' @template lightness
#' @param a,b color components, numeric vectors with values in \code{[-1, 1]}. (a=-1, b=-1) is blue, (a=-1, b=1) is green, (a=1, b=1) is red, (a=1, b=-1) is purple.
#'
#' @template color_spec
#' @template color_spec_from_matrix
#'
#' @details
#' The CIE L*a*b* color space is intended to represent all colors visible by the human eye. It separates a lightness component (\code{l}), which matches the human perception of lightness well, from two color "opponents" (\code{a} and \code{b}). To create color palettes however, the HCL color model (function \code{\link{hcl}}) is likely to be more practical and is also intended to match human perception of colors.
#' 
#' @examples
#' lab()
#' lab(0.47, 0.55, 0.3)
#' lab(data.frame(c(0.4, 0.6), c(-1, 1), c(-1, 1)))
#' lab(matrix(c(0.4, 0.6, -1, 1, -1, 1), ncol=3))
#' lab(0.5, c(-1, 1), c(-1, 1))
#'
#' # nice lightness scales
#' show_col(lab(l=seq(0, 1, length.out=10)))
#'
#' # represent slices of the color space, projected in R's sRGB representation
#' ab <- expand.grid(a=seq(-1, 1, length.out=10), b=seq(-1, 1, length.out=10))
#' plot(ab$a, ab$b, col=lab(l=0.5, a=ab$a, b=ab$b), pch=15, cex=3, asp=1)
#' plot(ab$a, ab$b, col=lab(l=0.2, a=ab$a, b=ab$b), pch=15, cex=3, asp=1)
#' plot(ab$a, ab$b, col=lab(l=1, a=ab$a, b=ab$b), pch=15, cex=3, asp=1)
#'
#' @export
lab <- function(l=0.5, a=0.5, b=0.5) {
  # handle color channels
  x <- tabularise_arguments(l, a, b)

  # parse colors using chroma.js
  colors <- parse_color(x, "lab")
  
  return(colors)
}
