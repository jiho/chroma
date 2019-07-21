#' Get or set a color channel
#'
#' Extract the value of a color channel or set it and therefore modify the color.
#'
#' @template param_x_rcolors
#' @param model string defining the color model; valid models are \code{cmyk}, \code{hcl}, \code{lch}, \code{hsi}, \code{hsl}, \code{hsv}, \code{lab}, \code{rgb}, and \code{rgba}.
#' @param channel string defining the channel within the color model.
#' @param value the channel value to set; a number, the convention of which depends on the channel.
#'
#' @return
#' For \code{channel}, a vector of channel values, the convention of which depend on the channel.
#'
#' For \code{channel <-}, the updated object(s).
#'
#' @seealso \code{\link{luminance}} for relative brightness, which is slightly different from the perceived brightness (i.e. the value of the L channel in L*a*b* or HCL).
#' @family color manipulation functions
#'
#' @export
#'
#' @examples
#' channel("pink", "rgb", "r")
#' channel(colors()[1:5], "hcl", "l")
#'
#' # Colors along a HSV rainbow have very different lightness,
#' # which makes them inapropriate for continuous color scales
#' # In contrast, lightness should be constant along a HCL rainbow
#' HSV_rainbow <- hsv(h=seq(0, 360, length.out=7), s=1, v=1)
#' HCL_rainbow <- hcl(h=seq(0, 360, length.out=7), l=0.65)
#'
#' show_col(HSV_rainbow, HCL_rainbow)
#' channel(HCL_rainbow, "hcl", "l")
#' # NB: the subtle changes are caused by the round-trip conversion to R's
#' #     internal sRGB representation of colors.
#' # make it more visual
#' plot(channel(HSV_rainbow, "hcl", "l"), col=HSV_rainbow,
#'      pch=19, cex=2, type="b", ylab="Lightness")
#' abline(h=0.65, lty="dotted")
#' points(channel(HCL_rainbow, "hcl", "l"), col=HCL_rainbow,
#'        pch=19, cex=2, type="b")
#'
#' # Force a given lightness
#' x_orig <- x <- HSV_rainbow
#' channel(x, "hcl", "l") <- 0.5
#' show_col(x_orig, x)
#'
#' # Make all colors equally saturated
#' x_orig <- x <- c("aliceblue", "aquamarine4", "coral", "blanchedalmond")
#' channel(x, "hsv", "s") <- 0.5
#' show_col(x_orig, x)
#'
#' # Keep the lightness and saturation but change the hue
#' # (also called "colorizing")
#' x_orig <- x <- c("aliceblue", "aquamarine4", "coral", "blanchedalmond")
#' channel(x, "hsv", "h") <- 240  # make all blue
#' show_col(x_orig, x)
channel <- function(x, model, channel) {
  # check arguments
  model <- match.arg(model, c("cmyk", "hcl", "hsi", "hsl", "hsv", "lab", "lch", "rgb", "rgba", "ryb", "css", "hex", "temperature", "wavelength"))
  if (model %in% c("css", "hex", "temperature", "wavelength")) {
    stop(paste0("Cannot extract a channel from a ", model, " color"))
  }

  # convert to the the given model
  x <- convert_color(x, model)

  # extract the channel
  channel <- match.arg(channel, colnames(x))
  x[,channel]
}

#' @rdname channel
#' @export
`channel<-` <- function(x, model, channel, value) {
  # check arguments
  model <- match.arg(model, c("cmyk", "hcl", "hsi", "hsl", "hsv", "lab", "lch", "rgb", "rgba", "ryb", "css", "hex", "temperature", "wavelength"))
  if (model %in% c("css", "hex", "temperature", "wavelength")) {
    stop(paste0("Cannot set a channel in a ", model, " color"))
  }

  # convert to the the given model
  x <- convert_color(x, model)

  # set the channel
  channel <- match.arg(channel, colnames(x))
  x[,channel] <- value

  # reconvert to R colors
  x <- parse_color(x, model)

  return(x)
}
