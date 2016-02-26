#' Get or set color channel
#'
#' @template param_x_rcolors
#' @template param_model
#' @param channel string defining the channel within the color model.
#' @param value the channel value to set; a number, the convention of which depend on the channel.
#'
#' @return
#' For \code{channel}, a vector of channel values, the convention of which depend on the channel.
#' 
#' For \code{channel<-}, the updated object(s).
#' 
#' @seealso \code{\link{luminance}} for relative brightness, which is slightly different from the perceived brightness (i.e. the value of the L channel in Lab or HCL).

#' @examples
#' channel("pink", "rgb", "r")
#' channel(colors()[1:5], "hcl", "l")
#'
#' # colors along a HSV rainbow have very different lightness
#' channel(rainbow(6), "hcl", "l")
#' # but lightness should be constant along a HCL "rainbow", by definition
#' # here subtle changes are caused by the round-trip conversion to R colors
#' channel(hcl(h=seq(0, 360, length.out=6), l=0.5), "hcl", "l")
#'
#' # force a given lightness
#' x_orig <- x <- rainbow(6)
#' show_col(x)
#' channel(x, "hcl", "l") <- 0.3
#' show_col(x_orig, x)
#'
#' @export
channel <- function(x, model, channel) {
  if (model == "css") {
    stop("Cannot extract a channel from a css color")
  }
  
  # convert to the the given model
  x <- convert_color(x, model)
  
  # extract the channel
  channel <- match.arg(channel, colnames(x))
  x[,channel] 
}

#' @name channel
#' @export
`channel<-` <- function(x, model, channel, value) {
  if (model == "css") {
    stop("Cannot set a channel in a css color")
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
