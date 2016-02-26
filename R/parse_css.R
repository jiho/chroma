#' CSS Color Specification
#'
#' Converts colors specified as CSS strings into R colors. This includes all colors in the X11 specification of the W3C \url{http://www.w3.org/TR/css3-color/#svg-color} as well as \code{rgb()} and \code{hsl()} constructs (but, in this case, it is probably easier to use the functions \code{\link{rgb}}) and \code{\link{hsl}} directly).
#'
#' @param x string representing a color in CSS.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @examples
#' css("teal")
#' css(c("teal", "blanchedalmond"))
#' css("rgb(255, 100, 100)")
#' css("hsl(200, 50%, 50%)")
#'
#' @export
css <- function(x) {
  parse_color(x, "css")
}
