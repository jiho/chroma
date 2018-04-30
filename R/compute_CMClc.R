#' Compute the CMC l:c difference between two colors
#'
#' Compute the difference between two colors, using the CMC l:c metric. \url{https://en.wikipedia.org/wiki/Color_difference#CMC_l:c_(1984)}.
#'
#' @template param_xy_rcolors
#' @param l,c weighting factors for lightness and chromacity.
#'
#' @return A number quantifying the difference between x and y (or a vector thereof).
#'
#' @export
#'
#' @seealso \code{\link{deltaE}} for another (probably more consistent) metric of difference between colors and \code{\link{color_distance}} for a numerical definition of the distance between two colors.
#'
#' @examples
#' CMClc("pink", "hotpink")
#' CMClc("pink", "blue")
#'
#' # NB: his definition of distance is not symetrical
#' CMClc("pink", "blue")
#' CMClc("blue", "pink")
#'
#' # The computation can be vectorised
#' # For example to find the closest color in an array of possibilities
#' clrs <- rainbow(20)
#' show_col("pink", clrs)
#' d <- CMClc("pink", clrs)
#' show_col("pink", clrs[which.min(d)])
CMClc <- function(x, y, l=1, c=1) {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)

  # check argument length
  foo <- tabularise_arguments(x, y)

  cmds <- stringr::str_c("chroma.deltaE('", x, "','", y, "',", l, ",", c, ")")
  as.numeric(v8_eval(cmds))
}
