#' Named R Color Specification
#' 
#' Convert a named R color as defined in \code{\link[grDevices]{colors}} into its hex representation
#' 
#' @param x vector of named R colors (can include colors in already hex representation)
#'
#' @examples
#' chroma_r("pink")
#' chroma_r(c("pink", "darkblue"))
#' 
#' show_col(chroma_r(colors()))
#'  
#' @export
chroma_r <- function(x) {
  rgb(t(col2rgb(x)), maxColorValue=255)
}
