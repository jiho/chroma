#' CIE L* a* b* Color Specification
#'
#' Create a vector of colors from lightness and a and b color-opponents.
#'
#' @template lightness
#' @param a,b color components, numeric vectors with values in \code{[-1, 1]}. (a=-1, b=-1) is blue, (a=-1, b=1) is green, (a=1, b=1) is red, (a=1, b=-1) is purple.
#'
#' @template color_spec
#'
#' @examples
#' lab()
#' lab(0.47, 0.55, 0.3)
#' lab(data.frame(c(0.4, 0.6), c(-1, 1), c(-1, 1)))
#' lab(matrix(c(0.4, 0.6, -1, 1, -1, 1), ncol=3))
#' lab(0.5, c(-1, 1), c(-1, 1))
#'
#' # nice color scales
#' show_col(lab(l=seq(0, 1, length.out=10)))
#'
#' # represent slices of the color space
#' ab <- expand.grid(a=seq(-1, 1, length.out=10), b=seq(-1, 1, length.out=10))
#' plot(ab$a, ab$b, col=lab(l=0.5, a=ab$a, b=ab$b), pch=15, cex=3, asp=1)
#' plot(ab$a, ab$b, col=lab(l=0.2, a=ab$a, b=ab$b), pch=15, cex=3, asp=1)
#' plot(ab$a, ab$b, col=lab(l=1, a=ab$a, b=ab$b), pch=15, cex=3, asp=1)
#'
#' @export
lab <- function(l=0.5, a=0.5, b=0.5) {
  if ( is.matrix(l) | is.data.frame(l) ) {
    # if first argument is a table, use this only
    d <- l[,1:3]
  } else {
    # else, convert the arguments into a table
    # NB: this does argument recycling on its own; we just provide a nicer error message
    tryCatch(d <- data.frame(l, a, b),
      error=function(e) {
        stop("The lengths of arguments l, a, and b are not compatible.", call.=F)
      }
    )
  }
  
  # check that argument values are correct
  if (any(d[,1] < 0)) {
    stop("l cannot be < 0")
  }
  if (any(d[,1] > 1)) {
    stop("l cannot be > 1")
  }
  if (any(d[,2] < -1)) {
    stop("a cannot be < -1")
  }
  if (any(d[,2] > 1)) {
    stop("a cannot be > 1")
  }
  if (any(d[,3] < -1)) {
    stop("b cannot be < -1")
  }
  if (any(d[,3] > 1)) {
    stop("b cannot be > 1")
  }
  
  # chroma.js works with percentages here
  d <- d * 100
  
  # convert each row of input
  apply(d, 1, function(x) {
    chroma.lab(x[1], x[2], x[3])
  })
}


chroma.lab <- function(l, a, b) {
  ct <- v8_chroma_context()
  cmd <- paste0("chroma.lab(", l, ",", a, ",", b, ").hex()")
  ct$eval(cmd)
}
