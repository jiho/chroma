#' RGB Color Specification
#'
#' Create a vector of colors from red, green, and blue.
#'
#' @param red,green,blue color channels, numeric vectors with values in \code{[0, maxColorValue]}.
#' @param alpha transparency, numeric vector with values in \code{[0, maxColorValue]}; 0 means fully transparent, \code{maxColorValue} means fully opaque. See function \code{\link{alpha}} for another way to change the transparency after the fact.
#' @param names character vector. The names for the resulting vector.
#' @param maxColorValue number giving the maximum of the color values range; typically 1 or 255.
#'
#' @template color_spec
#' @template color_spec_from_matrix
#'
#' @details
#' \code{rgba} is a variant which forces \code{maxColorValue} to be 1 and allows to specify an alpha value as a color component (i.e. as the fourth column of the first argument when this argument is a matrix/data.frame). \code{gl} is a synonym of \code{rgba} for compatibility with chroma.js.
#'
#' RGB is how colors are displayed on a computer screen. However, this is not how colors are perceived by the human eye/brain. Other color spaces such as HCL and L* a* b* make it easier to create color palettes that are appropriate for human perception.
#'
#' @export
#'
#' @examples
#' rgb()
#' rgb(1, 0, 0)
#' rgb(255, 0, 0, maxColorValue=255)
#'
#' red <- rgb(1, 0, 0)
#' transparent_red <- rgb(1, 0, 0, alpha=0.7)
#' show_col(c(red, transparent_red))
#'     
#' ramp <- seq(0, 1, length.out=10)
#' rgb(red=ramp, green=0, blue=0, names=paste("red", 1:10, sep="."))
#'
#' show_col(
#'   rgb(r=ramp),
#'   rgb(g=ramp),
#'   rgb(b=ramp)
#' )
rgb <- function(red=0, green=0, blue=0, alpha=NULL, names=NULL, maxColorValue=1) {

  # handle color channels
  x <- tabularise_arguments(red, green, blue)

  # scale color channels
  x <- round(x / maxColorValue * 255)
  # NB: rgb is integer in [0,255 in chroma.js
  
  # parse colors using chroma.js
  colors <- parse_color(x, "rgb")
  
  # add transparency if needed
  if ( !is.null(alpha) ) {
    if ( !(length(alpha) == 1 | length(alpha) == length(colors)) ) {
      stop("alpha needs to be either a single number or a vector of the same length as the number of colors (", length(colors), " here).")
    }
    alpha <- alpha / maxColorValue
    colors <- alpha(colors, alpha)
  }
  
  # add names if needed
  if ( !is.null(names) ) {
    names(colors) <- names
  }
  
  return(colors)
}

#' @name rgb
#' @export
rgba <- function(red=0, green=0, blue=0, alpha=1) {
  # handle color channels
  x <- tabularise_arguments(red, green, blue, alpha)

  # parse colors using chroma.js
  colors <- parse_color(x, "gl")
  
  # add alpha channel
  colors <- alpha(colors, x[,4])

  return(colors)
}

#' @name rgb
#' @export
gl <- rgba
