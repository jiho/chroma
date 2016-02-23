#' Force HEX representation
#' 
#' Convert R colors, including named ones as defined in the function \code{\link[grDevices]{colors}}, into their hex representation
#' 
#' @template rcolors
#'
#' @return A vector of colors specified as hex codes only
#'
#' @examples
#' in_hex("pink")
#' in_hex(c("pink", "darkblue"))
#' 
#' show_col(in_hex(colors()))
#'  
#' @export
in_hex <- function(x) {
  grDevices::rgb(t(grDevices::col2rgb(x)), maxColorValue=255)
}
