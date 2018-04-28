#' Get or set color channel
#'
#' @template param_x_rcolors
#' @template param_model
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
#' channel(rainbow(6), "hcl", "l")
#' # But, by definition, lightness should be constant along a HCL "rainbow"
#' channel(hcl(h=seq(0, 360, length.out=6), l=0.5), "hcl", "l")
#' # NB: subtle changes are caused by the round-trip conversion to R colors.
#'
#' # Force a given lightness
#' x_orig <- x <- rainbow(6)
#' channel(x, "hcl", "l") <- 0.3
#' show_col(x_orig, x)
#'
#' # Make all colors equally saturated
#' x_orig <- x <- c("aliceblue", "aquamarine4", "coral", "blanchedalmond")
#' channel(x, "hsv", "s") <- 0.5
#' show_col(x_orig, x)
#'
#' # Keep the lightness and saturation but change the hue
#' # (also called "colorizing")
#'
#' x_orig <- x <- c("aliceblue", "aquamarine4", "coral", "blanchedalmond")
#' channel(x, "hsv", "h") <- 240  # make all blue
#' show_col(x_orig, x)
channel <- function(x, model, channel) {
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
  if (model %in% c("css", "hex", "temperature", "wavelength")) {
    stop(paste0("Cannot extract a channel in a ", model, " color"))
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
