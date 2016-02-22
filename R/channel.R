#' Get or set color channel
#'
#' @template colors
#' @template model
#' @param channel string defining the channel within the color model
#'
#' @examples
#' channel("pink", "rgb", "r")
#' col <- rainbow(5)
#' show_col(col)
#' channel(col, "hcl", "l") <- 0.3
#' show_col(col)
#' channel(colors()[1:5], "hcl", "l")
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
