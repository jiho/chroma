#' CSS Color Specification
#'
#' Converts colors specified as CSS strings into R colors. This includes all colors in the X11 specification of the W3C \code{<http://www.w3.org/TR/css3-color/#svg-color>} as well as \code{rgb()} and \code{hsl()} constructs (but, in this case, it is probably easier to use the functions \code{\link[grDevices]{rgb}}) and \code{\link{hsl}} directly).
#'
#' @param color string representing a color in CSS.
#'
#' @family color specifications
#'
#' @return A vector of colors specified as hex codes
#'
#' @examples
#' css_color("teal")
#' css_color(c("teal", "blanchedalmond"))
#' css_color("rgb(255, 100, 100)")
#' css_color("hsl(200, 50%, 50%)")
#'
#' @export
css_color <- function(x) {
  # check argument
  x <- as.character(x)
  
  # convert each row of input
  sapply(x, function(xx) {
    chroma.css(xx)
  }, USE.NAMES=FALSE)
}


chroma.css <- function(x) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.css('", x, "').hex()")
  ct$eval(cmd)
}
