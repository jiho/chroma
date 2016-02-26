#' HEX Color Specification
#'
#' Converts colors specified as hexadecimal strings into R colors. This includes the usual format for R colors but also the shorter three letter format. In addition the \code{#} sign can be omitted.
#'
#' @param x hexadecimal string specifying a color.
#'
#' @details
#' Hexadecimal strings (hex for short) are how colors are usually represented in R. This function just brings a little more versatility in how those can be specified. Note, however, that R can use the 8 letter model to represent color with a transparency (alpha) component and that this function does not support it (and just removes the transparency).
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @examples
#' hex("#ff3399")
#' hex("ff3399")
#' hex("ff3399")
#' hex("F39")
#' 
#' (half_transparent <- alpha("#ff3399", 0.5))
#' hex(half_transparent)
#' # the transparency portion (last two positions: '80') is removed
#'
#' @export
hex <- function(x) {
  parse_color(x, "hex")
}
