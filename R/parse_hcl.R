#' HCL Color Specification
#'
#' Create a vector of colors from hue, chromacity, and lightness. The arguments are compatible with the \code{\link[grDevices]{hcl}} function, included with R in package \code{grDevices}, but the conventions and output are slightly different.
#'
#' @template param_hue
#' @template param_chromacity
#' @template param_lightness
#' @template param_alpha
#' @param ... ignored, for compatibility with the built-in \code{\link[grDevices]{hcl}} function.
#'
#' @template color_spec_from_matrix
#' @template details_hcl
#'
#' @template return_hex_colors
#'
#' @seealso HCL-based color scales: \code{\link{hue_scale}}, \code{\link{chroma_scale}}, and \code{\link{light_scale}}.
#' @template color_spec
#'
#' @examples
#' hcl()
#' hcl(330, 1, 0.6)
#' hcl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hcl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hcl(c(330, 340), 1, 0.6)
#'
#' # Compare color palettes
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hcl(h=ramp*360), hsv(h=ramp*360, s=0.9), hsl(h=ramp*360, s=0.9))
#' show_col(hcl(c=ramp))
#' show_col(hcl(l=ramp))
#'
#' # Nice color palettes
#' show_col(
#'   hcl(h=80+ramp*240, c=0.2+ramp*0.4, l=0.9-ramp*0.6),
#'   hcl(h=240-ramp*120, c=0.5, l=ramp*0.9),
#'   hcl(h=0+ramp*80, c=0.6-ramp*0.4, l=0.1+ramp*0.8),
#'   hcl(h=210+ramp*150, c=0.3, l=0.1+ramp*0.5)
#' )
#'
#' @export
hcl <- function(h=0, c=0.65, l=0.65, alpha=NULL, ...) {
  # TODO c and l are in 0:100 in grDevices. make it compatible
  # handle color channels
  x <- tabularise_arguments(h, c, l)

  # parse colors using chroma.js
  colors <- parse_color(x, "hcl")

  # add transparency if needed
  if ( !is.null(alpha) ) {
    if ( !(length(alpha) == 1 | length(alpha) == length(colors)) ) {
      stop("alpha needs to be either a single number or a vector of the same length as the number of colors (", length(colors), " here).")
    }
    colors <- alpha(colors, alpha)
  }
    
  return(colors)
}

#' @name hcl
#' @export
lch <- function(l=0.65, c=0.65, h=0) {
  # handle color channels
  x <- tabularise_arguments(l, c, h)

  # parse colors using chroma.js
  colors <- parse_color(x, "lch")
    
  return(colors)
}
