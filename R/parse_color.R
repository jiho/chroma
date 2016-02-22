#' Parse colors specified in a given model
#'
#' @param x a matrix or data.frame whose columns specify the color channels.
#' @template model
#'
#' @family color specifications
#'
#' @return A vector of colors specified as hex codes
#'
#' @examples
#' parse_color(data.frame(h=c(0, 120, 240), s=0.5, v=0.7), "hsv")
#' parse_color(data.frame(r=c(255, 0, 0),
#'                        g=c(0, 255, 0),
#'                        b=c(0, 0, 255)), "rgb")
#'
#' @export
parse_color <- function(x, model) {
  # recognise color model
  model <- match.arg(model, c("rgb", "hsv", "hsl", "hcl", "lch", "lab", "cmyk", "css"))

  # check arguments
  if (model == "css") {
    x <- as.character(x)
    # TODO harder check here
  } else {
    
    if (!(is.matrix(x) | is.data.frame(x))) {
      stop("x should be a matrix or data.frame")
    }

    required_columns <- switch(model, cmyk = 4, 3)
    if (ncol(x) < required_columns) {
      stop("x should have at least ", required_columns, " columns")
    }
    if (ncol(x) > required_columns) {
      warning("only the first ", required_columns, " columns of x will be used")
    }

  }
  
  # the range of some channels is homogenised in this package (all "percentage" are between 0 and 1)
  # modify the values here to match their definition in chroma.js (which are not homogeneous)
  if ( model == "hcl" ) {
    x[,2:3] <- x[,2:3] * 100
  } else if ( model == "lch" ) {
    x[,1:2] <- x[,1:2] * 100
  } else if ( model == "lab" ) {
    x <- x * 100
  }
  
  # parse colors using chroma.js
  cmds <- apply(x, 1, function(xx) {
    paste0("chroma.", model, "([", paste0(xx, collapse=","), "]).hex()")
  })
  res <- v8_eval(cmds)
  
  return(res)
}
