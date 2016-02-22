#' Color Temperature Specification
#'
#' Converts color temperature in Kelvin to a color vector.
#'
#' @param temperature light temperature in Kelvin, a numeric vector with values in \code{[1000, 40000]}. 1000 is candle light, 2000 is sunset light, 3200 is a tungsten light bulb, 4000 is a white fluorescent light tube, 5200 is average midday sunlight, 7000 is overcast daylight, 10000 is the color of a clear blue sky.
#'
#' @family color specifications
#'
#' @return A vector of colors specified as hex codes
#'
#' @examples
#' color_temperature()
#' color_temperature(c(2000, 5200, 7000))
#'
#' # display range
#' temp <- seq(1000, 20000, length.out=20)
#' plot(x=temp, y=rep(1, 20), col=color_temperature(temp), pch=15, cex=3)
#'
#' @export
color_temperature <- function(temperature=5200) {
  
  # check that argument values are correct
  if (any(temperature < 1000)) {
    stop("temperature cannot be < 1000")
  }
  if (any(temperature > 40000)) {
    stop("temperature cannot be > 40000")
  }
  
  # convert each row of input
  sapply(temperature, function(x) {
    chroma.temperature(x)
  }, USE.NAMES=FALSE)
}


chroma.temperature <- function(x) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.temperature(", x, ").hex()")
  ct$eval(cmd)
}
