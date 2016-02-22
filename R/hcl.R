#' HCL Color Specification
#'
#' Create a vector of colors from hue, chroma, and lightness.
#'
#' @template hue
#' @param c chroma, numeric vector with values in \code{[0, 1]}; 0 is grey, 1 is full color.
#' @template lightness
#'
#' @template color_spec
#'
# TODO: add advice such as the following in all functions
# In HCL space, the perceived color (hue) is completely separated from the perceived lightness of the color. All colors on a pure hue scale (same \code{c} and \code{l}) have the same apparent brightness.
#'
#' @examples
#' hcl()
#' hsl(330, 1, 0.6)
#' hsl(data.frame(c(330, 340), c(1,1), c(0.6, 0.4)))
#' hsl(matrix(c(330, 340, 1, 1, 0.6, 0.4), ncol=3))
#' hsl(c(330, 340), 1, 0.6)
#'
#' # color ramps
#' ramp <- seq(0, 1, length.out=10)
#' show_col(hcl(h=ramp*360))
#' show_col(hcl(c=ramp))
#' show_col(hcl(l=ramp))
#'
#' @export
hcl <- function(h=0, c=0.6, l=0.6) {
  if ( is.matrix(h) | is.data.frame(h) ) {
    # if first argument is a table, use this only
    d <- h[,1:3]
  } else {
    # else, convert the arguments into a table
    # NB: this does argument recycling on its own; we just provide a nicer error message
    tryCatch(d <- data.frame(h, c, l),
      error=function(e) {
        stop("The lengths of arguments h, c, and l are not compatible.", call.=F)
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
    stop("c cannot be < 0")
  }
  if (any(d[,2] > 1)) {
    stop("c cannot be > 1")
  }
  if (any(d[,3] < 0)) {
    stop("l cannot be < 0")
  }
  if (any(d[,3] > 1)) {
    stop("l cannot be > 1")
  }
  
  # chroma.js works with percentages here
  d[,2:3] <- d[,2:3] * 100
  
  # convert each row of input
  apply(d, 1, function(x) {
    chroma.hcl(x[1], x[2], x[3])
  })
}


chroma.hcl <- function(h, c, l) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.hcl(", h, ",", c, ",", l, ").hex()")
  ct$eval(cmd)
}

#' @name hcl
#' @export
lch <- function(l=0.5, c=0.5, h=0) {
  hcl(h, c, l)
}
