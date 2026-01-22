#' HEX color specification
#'
#' Converts colors specified as hexadecimal strings into R colors. This includes the usual format for R colors but also the shorter three letters format. In addition the \code{#} sign can be omitted.
#'
#' @param x hexadecimal string specifying a color; named R or CSS color are also tolerated.
#'
#' @details
#' Hexadecimal strings (hex for short) are how colors are usually represented in R. This function just brings a little more versatility in how those can be specified. Note, however, that R can use the 8 letter model to represent color with a transparency (alpha) component and that this function does not support it (and just removes the transparency).
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @family color specifications
#'
#' @export
#'
#' @examples
#' hex("#ff3399")
#' hex("ff3399")
#' hex("ff3399")
#' hex("F39")
#' x <- c("#ff3399",       # hex specification valid in R
#'        "F39",           # hex specification not valid in R
#'        "whitesmoke",    # named color valid in R and CSS
#'        "darkorchid2"   # named R color (not existing in CSS)
#'        )
#' hex(x)
#'
#' (half_transparent <- alpha("#ff3399", 0.5))
#' hex(half_transparent)
#' # NB: the transparency portion (last two positions: '80') is removed
hex <- function(x) {
  # match R colors
  ir <- x %in% grDevices::colors()
  x[ir] <- in_hex(x[ir])
  # parse the rest
  parse_color(x, "hex")
}
# TODO handle named CSS color (not existing in R)
# hex("rebeccapurple") throws error with "chroma.hex('rebeccapurple').hex()" in parse_color
# should be called with parse_color(x, "css")
