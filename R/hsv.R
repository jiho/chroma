#' HSV Color Specification
#'
#' Create a vector of colors from hue, saturation, and value.
#'
#' @template hue
#' @template saturation
#' @param v value, same conventions as \code{s}; 0 is black, 1 is full brightness
#'
#' @template color_spec
#'
#' @examples
#' hsv()
#' hsv(330, 1, 0.6)
#' hsv(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsv(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsv(c(330, 340), 1, 0.6)
#'
#' # color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hsv(h=ramp*360))
#' show_col(hsv(s=ramp))
#' show_col(hsv(v=ramp))
#'
#' @export
hsv <- function(h=0, s=0.6, v=0.7) {
  if ( is.matrix(h) | is.data.frame(h) ) {
    # if first argument is a table, use this only
    d <- h[,1:3]
  } else {
    # else, convert the arguments into a table
    # NB: this does argument recycling on its own; we just provide a nicer error message
    tryCatch(d <- data.frame(h, s, v),
      error=function(e) {
        stop("The lengths of arguments h, s, and v are not compatible.", call.=F)
      }
    )
  }
  
  # check that argument values are correct
  if (any(d[,1] < 0)) {
    stop("h cannot be < 0")
  }
  if (any(d[,1] > 360)) {
    stop("h cannot be > 360")
  }
  if (any(d[,2] < 0)) {
    stop("s cannot be < 0")
  }
  if (any(d[,2] > 1)) {
    stop("s cannot be > 1")
  }
  if (any(d[,3] < 0)) {
    stop("l cannot be < 0")
  }
  if (any(d[,3] > 1)) {
    stop("l cannot be > 1")
  }
  
  # convert each row of input
  apply(d, 1, function(x) {
    chroma.hsv(x[1], x[2], x[3])
  })
}


chroma.hsv <- function(h, s, v) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.hsv(", h, ",", s, ",", v, ").hex()")
  ct$eval(cmd)
}
