#' Compute the Euclidean distance between two colors
#'
#' Compute the Euclidean distance between two colors in a given color space.
#'
#' @template param_xy_rcolors
#' @param model string defining the color model; valid models are \code{cmyk}, \code{hcl}, \code{lch}, \code{hsi}, \code{hsl}, \code{hsv}, \code{lab}, \code{rgb}, and \code{rgba}.
#'
#' @return The numerical distance between x and y (or a vector thereof).
#'
#' @export
#'
#' @seealso \code{\link{deltaE}} and \code{\link{CMClc}} for perceptual, rather than numerical, distance between colors.
#'
#' @examples
#' color_distance("pink", "hotpink")
#' color_distance("pink", "blue")
#'
#' # The absolute value of the distance depends on the color space
#' # it is computed in but the relative changes still make sense
#' color_distance("pink", "hotpink", c("cmyk", "hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "rgba"))
#' color_distance("pink", "blue", c("cmyk", "hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "rgba"))
#'
#' # Find the closest color in an array of possibilities
#' clrs <- rainbow(20)
#' show_col("pink", clrs)
#' # use various models
#' dhsv <- color_distance("pink", clrs, "hsv")
#' dlab <- color_distance("pink", clrs, "lab")
#' drgb <- color_distance("pink", clrs, "rgb")
#' # find closest colors in each space
#' match_hsv <- clrs[which.min(dhsv)]
#' match_lab <- clrs[which.min(dlab)]
#' match_rgb <- clrs[which.min(drgb)]
#' # display them
#' show_col(
#'   "pink", clrs,
#'   c("pink", match_hsv),
#'   c("pink", match_lab),
#'   c("pink", match_rgb)
#' )
#' # plot the values of the differences
#' plot(dhsv, pch=19, col=match_hsv)
#' points(dlab, pch=19, col=match_lab)
#' points(drgb, pch=19, col=match_rgb)
#' legend(1,50,legend=c("hsv", "lab", "rgb"), col=c(match_hsv, match_lab, match_rgb), pch=19)
#' # The rainbow is defined in HSV so the distance varies linearly (only
#' # H varies) and the match in terms of hue is best. The variations in L*a*b*
#' # and RGB are similar but different components weight differently. For
#' # L*a*b*, luminance and chroma seem to import more since the hue of the
#' # match is completely off.
color_distance <- function(x, y, model="lab") {
  # force input R colors into hex notation
  x <- in_hex(x)
  y <- in_hex(y)
  # check model argument
  model <- match.arg(model, c("cmyk", "hcl", "lch", "hsi", "hsl", "hsv", "lab", "rgb", "rgba"), several.ok=TRUE)

  # check argument length
  foo <- tabularise_arguments(x, y, model)

  cmds <- stringr::str_c("chroma.distance('", x, "','", y, "','", model, "')")
  as.numeric(v8_eval(cmds))
}
#' @rdname color_distance
#' @export
cdistance <- color_distance
