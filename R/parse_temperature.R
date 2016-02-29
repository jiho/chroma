#' Color Temperature Specification
#'
#' Converts color temperature in Kelvin to a color vector.
#'
#' @param temperature light temperature in Kelvin, a numeric vector with values in \code{[1000, 40000]}. 1000 is candle light, 2000 is sunset light, 3200 is a tungsten light bulb, 4000 is a white fluorescent light tube, 5200 is average midday sunlight, 7000 is overcast daylight, 10000 is the color of a clear blue sky.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @examples
#' temperature()
#' temperature(c(2000, 5200, 7000))
#'
#' # Display the available range of color temperatures
#' x <- seq(1000, 20000, length.out=20)
#' plot(x=x, y=rep(1, 20), col=temperature(x), pch=15, cex=3)
#'
#' @export
temperature <- function(temperature=5200) {
  parse_color(temperature, "temperature")
}
