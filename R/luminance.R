#' Get or set the relative brightness of colors
#'
#' @details
#' \code{luminance} gets the relative brightness of colors, normalised to 0 for darkest black and 1 for lightest white according to the WCAG definition \url{http://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef}.
#'
#' \code{luminance<-} sets the brightness by interpolating with black or white until the appropriate relative luminance is found.
#' 
#' @template param_x_rcolors
#' @param value the luminance value, as a number in [0,1]
#'
#' @return
#' For \code{luminance}, a vector of luminance values, each in [0,1].
#' 
#' For \code{luminance<-}, the updated object(s).
#'
#' @seealso \code{\link{channel}} to extract or set a given channel, in particular the lightness channel in HSL, HCL, or L*a*b* color spaces.
#' @family color manipulation functions
#' 
#' @examples
#' luminance("black")
#' luminance("white")
#' luminance("red")
#'
#' # Colors along a HSV rainbow have very different perceived brightness
#' luminance(rainbow(6))
#' # but brightness is much more homogenous along a HCL "rainbow"
#' luminance(hcl(h=seq(0, 360, length.out=6)))
#'
#' # Change the luminance
#' col <- "red"
#' luminance(col) <- 0.5
#' show_col(c("red", col))
#' 
#' # This can even be used to create color ramps, although the steps in
#' # brightness may not be regular at the extremes
#' col <- "red"
#' luminance(col) <- seq(0, 1, length.out=10)
#' show_col(col)
#' # Better to avoid the extremes then
#' col <- "red"
#' luminance(col) <- seq(0.1, 0.9, length.out=10)
#' show_col(col)
#'
#' @export
luminance <- function(x) {
  # force input R colors into hex notation
  x <- in_hex(x)

  # extract luminance
  cmds <- paste0("chroma('", x, "').luminance()")
  res <- v8_eval(cmds)
  
  # convert to numbers
  res <- as.numeric(res)
  
  return(res)
}

#' @name luminance
#' @export
`luminance<-` <- function(x, value) {
  # force input R colors into hex notation
  x <- in_hex(x)

  # set luminance
  cmds <- paste0("chroma('", x, "').luminance(", value ,").hex()")
  res <- v8_eval(cmds)
}
