#' HCL Color Specification
#'
#' Create a vector of colors from hue, chromacity, and lightness.
#'
#' @template param_hue
#' @template param_chromacity
#' @template param_lightness
#' @template param_alpha
#' @param compat whether to make the conventions compatible with the built-in function \code{grDevices::\link[grDevices]{hcl}}: \code{c} and \code{l} should be in [0,100], not [0,1]. However, the mechanism for converting from HCL space to sRGB are different in the two functions, and they therefore produce different colors.
#' @param ... ignored, for compatibility with the built-in \code{\link[grDevices]{hcl}} function.
#'
#' @template color_spec_from_matrix
#' @template details_hcl
#'
#' @template return_hex_colors
#'
#' @seealso HCL-based color scales: \code{\link{hue_scale}}, \code{\link{chroma_scale}}, and \code{\link{light_scale}}.
#'
#' @template color_spec
#'
#' @export
#'
#' @examples
#' hcl()
#' hcl(330, 1, 0.6)
#' hcl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hcl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hcl(c(330, 340), 1, 0.6)
#'
#' # Compare "rainbow"-like scales in various color spaces
#' ramp <- seq(0, 1, length.out=10)
#' show_col(
#'   hcl(h=ramp*360),
#'   hsv(h=ramp*360, s=0.9),
#'   hsl(h=ramp*360, s=0.9)
#' )
#' # Hue, chromacity, and lightness scales
#' show_col(
#'   hcl(h=ramp*360),
#'   hcl(c=ramp),
#'   hcl(l=ramp)
#' )
#'
#' # Nice color palettes
#' show_col(
#'   hcl(h=80+ramp*240, c=0.2+ramp*0.4, l=0.9-ramp*0.6),
#'   hcl(h=240-ramp*120, c=0.5, l=ramp*0.9),
#'   hcl(h=0+ramp*80, c=0.6-ramp*0.4, l=0.1+ramp*0.8),
#'   hcl(h=210+ramp*150, c=0.3, l=0.1+ramp*0.5)
#' )
hcl <- function(h=0, c=0.65, l=0.65, alpha=NULL, compat=FALSE, ...) {
  # handle color channels
  x <- tabularise_arguments(h, c, l)

  # make compatible with grDevices::hcl
  if (compat) {
    x[,2:3] <- x[,2:3] / 100
  }

  # parse colors using chroma.js
  colors <- parse_color(x, "hcl")

  # add transparency if needed
  if ( !is.null(alpha) ) {
    colors <- alpha(colors, alpha)
  }

  return(colors)
}

#' @rdname hcl
#' @export
lch <- function(l=0.65, c=0.65, h=0) {
  # handle color channels
  x <- tabularise_arguments(l, c, h)

  # parse colors using chroma.js
  colors <- parse_color(x, "lch")

  return(colors)
}
