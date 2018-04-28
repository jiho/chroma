#' Color Temperature Specification
#'
#' Converts color temperature in Kelvin to a color vector.
#'
#' @param x light temperature in Kelvin, a numeric vector with values in \code{[1000, 40000]}. 1000 is candle light, 2000 is sunset light, 3200 is a tungsten light bulb, 4000 is a white fluorescent light tube, 5200 is average midday sunlight, 7000 is overcast daylight, 10000 is the color of a clear blue sky.
#'
#' @template return_hex_colors
#'
#' @template color_spec
#'
#' @export
#'
#' @examples
#' temperature()
#' temperature(c(2000, 5200, 7000))
#'
#' # Display a large range of color temperatures
#' x <- seq(1000, 15000, length.out=200)
#' notable_temperatures <- c(1000, 2000, 3200, 4000, 5200, 7000, 10000)
#' plot(x=x, y=rep(0, 200), col=temperature(x), pch="|", cex=5, xaxt='n')
#' axis(side=1, at=notable_temperatures, labels=TRUE, las=2)
#' abline(v=notable_temperatures, lty="dotted")
temperature <- function(x=5200) {
  parse_color(x, "temperature")
}
