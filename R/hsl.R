#' HSL Color Specification
#'
#' Create a vector of colors from hue, saturation and lightness.
#'
#' @template hue
#' @template saturation
#' @param l lightness, same conventions as \code{s}
#'
#' @return A vector of colors specified as hex codes
#'
#' @family color specifications
#'
#' @examples
#' hsl()
#' hsl(330, 1, 0.6)
#' hsl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsl(c(330, 340), 1, 0.6)
#'
#' show_col(hsl(h=seq(0, 360, length.out=10)))
#'
#' @export
hsl <- function(h=0, s=0.5, l=0.5) {
  if ( is.matrix(h) | is.data.frame(h) ) {
    # if first argument is a table, use this only
    d <- h[,1:3]
  } else {
    # else, convert the arguments into a table
    # NB: this does argument recycling on its own; we just provide a nicer error message
    tryCatch(d <- data.frame(h, s, l),
      error=function(e) {
        stop("The lengths of arguments h, s, and l are not compatible.", call.=F)
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
    chroma.hsl(h=x[1], s=x[2], l=x[3])
  })
}


chroma.hsl <- function(h, s, l) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.hsl(", h, ",", s, ",", l, ").hex()")
  ct$eval(cmd)
}
